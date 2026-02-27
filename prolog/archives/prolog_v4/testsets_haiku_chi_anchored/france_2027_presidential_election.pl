% ============================================================================
% CONSTRAINT STORY: france_2027_presidential_election
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_france_2027_presidential_election, []).

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
 *   constraint_id: france_2027_presidential_election
 *   human_readable: 2027 French Presidential Election Framework
 *   domain: political/electoral_systems
 *
 * SUMMARY:
 *   The 2027 French presidential election is structurally defined by the
 *   Republican Barrier (Cordon Sanitaire), an informal institutional
 *   mechanism by which mainstream left and right parties coordinate to
 *   exclude far-right candidates from second-round viability. As of early
 *   2026, this constraint is exhibiting characteristic degradation:
 *   extractiveness rising (0.38 → 0.52), theater ratio increasing (0.42 →
 *   0.58), and the underlying consensus cost-benefit calculation shifting
 *   unfavorably for at least some coalition partners. The two-round system
 *   functions as the primary enforcement mechanism: first-round voting is
 *   free; second-round choices are constrained by the Barrier's binary
 *   structure. The far-right (Rassemblement National and variants) commands
 *   30-35% of first-round support but faces systematic exclusion in round two
 *   through mainstream party coalition. This creates a structural asymmetry:
 *   the far-right's voters are trapped (highest possible exit cost), while
 *   centrist and left actors bear moderate coalition costs but retain
 *   institutional arbitrage. The constraint exhibits properties of tangled
 *   rope (genuine coordination function preventing democratic backsliding,
 *   combined with extraction of voter expression) and piton (vestigial norm
 *   maintained through theater as underlying consensus erodes). The
 *   analytical question is not whether the Barrier will hold in 2027 (it
 *   likely will, mechanically), but whether it retains functional legitimacy
 *   or has become a performative ritual masking an unstable coalition.
 *
 * KEY AGENTS:
 *   - Far-Right Voter Base (RN, other right-populist movements): Primary victim (powerless/trapped) — commanding 30-35% first-round support but structurally excluded from viable second-round outcomes
 *   - Centrist-Establishment Coalition (Macronists, moderate Republicans): Primary beneficiary (institutional/arbitrage) — emerges as 'safe' round-two choice; benefits from Barrier coordination
 *   - Left-Wing Coalition (Socialist Party, NUPES, Greens): Secondary victim (moderate/constrained) — forced into coalition with centrist opponents; surrenders policy leverage in round two
 *   - Constitutional Framework (Fifth Republic, two-round system): Institutional actor (institutional/constrained) — both enables and constrains; functions as coordination mechanism but suppresses authentic expression
 *   - Republican Barrier Norm: Vestigial consensus (institutional/arbitrage) — maintained through elite coordination; increasingly performative as underlying consensus degrades
 *   - Electoral Reform Advocates: Organized agents (powerful/mobile) — view Barrier as temporary; position alternatives as permanent solutions
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — assesses whether constraint represents genuine democratic necessity or extraction disguised as civic virtue
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(france_2027_presidential_election, 0.52).
domain_priors:suppression_score(france_2027_presidential_election, 0.65).
domain_priors:theater_ratio(france_2027_presidential_election, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(france_2027_presidential_election, extractiveness, 0.52).
narrative_ontology:constraint_metric(france_2027_presidential_election, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(france_2027_presidential_election, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(france_2027_presidential_election, tangled_rope).
narrative_ontology:human_readable(france_2027_presidential_election, "2027 French Presidential Election Framework").
narrative_ontology:topic_domain(france_2027_presidential_election, "political/electoral_systems").

domain_priors:requires_active_enforcement(france_2027_presidential_election).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(france_2027_presidential_election, centrist_establishment).
narrative_ontology:constraint_beneficiary(france_2027_presidential_election, traditional_right_left_blocs).
narrative_ontology:constraint_victim(france_2027_presidential_election, electoral_authenticity).
narrative_ontology:constraint_victim(france_2027_presidential_election, far_right_supporters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FAR-RIGHT VOTER BASE (SNARE) — Trapped within the Republican Barrier framework. Despite commanding 30-35% of first-round support, the two-round system forces voters into binary choice constrained by centrist coordination. No exit option: voting their preference in round one triggers forced capitulation in round two. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(france_2027_presidential_election, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LEFT-WING COALITION (SNARE) — Constrained to participate in the Republican Barrier despite ideological friction with centrists. Must coalesce with centrist opponents to block the far-right in round two, surrendering negotiating power. Career costs for left politicians refusing to endorse centrist second-round winner. d≈0.78, f(d)≈1.08, σ=1.0 → χ≈0.56.
constraint_indexing:constraint_classification(france_2027_presidential_election, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRIST-ESTABLISHMENT COALITION (ROPE) — Benefits from the Republican Barrier as coordination mechanism. The two-round system and anti-far-right consensus enable centrist candidates (Macron bloc, moderate Republicans) to emerge victorious by being the 'safe' round-two choice. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05. Net beneficiary; experiences constraint as functional coordination.
constraint_indexing:constraint_classification(france_2027_presidential_election, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL FRAMEWORK (TANGLED ROPE) — The two-round presidential system functions as designed (coordination to block extremes) but simultaneously enforces extraction: suppresses first-round expression of voter intent, narrows policy space, and creates incentive for institutional actors to maintain artificial coalition costs. The framework itself is both beneficiary and victim—it enables governance but at cost of democratic authenticity. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.34.
constraint_indexing:constraint_classification(france_2027_presidential_election, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REPUBLICAN BARRIER NORM (PITON) — The norm that mainstream parties automatically coalesce to block the far-right in round two persists through institutional inertia, but its functional grip is weakening. theater_ratio=0.58 reflects that the barrier is increasingly performative: major party leaders signal commitment to the Republican Barrier while privately calculating whether losing is preferable to coalition costs. The ritual persists but the underlying consensus is degrading. The norm is maintained by elite coordination, not by voter demand or institutional necessity.
constraint_indexing:constraint_classification(france_2027_presidential_election, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ELECTORAL REFORM MOVEMENT (SCAFFOLD) — Some institutional actors and civil society organizations view the Republican Barrier as a temporary patch requiring structural reform. Ranked-choice voting, proportional representation, or other alternatives are positioned as permanent solutions. This perspective sees the current framework as coordination with a sunset: as alternatives mature politically, the necessity for the Barrier declines. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.18. Low effective extraction because mobile agents perceive a path to structural change.
constraint_indexing:constraint_classification(france_2027_presidential_election, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Views the Republican Barrier as genuine coordination (necessary to prevent democratic backsliding) combined with asymmetric extraction (suppression of voter expression and policy diversity). The constraint functions but at structural cost. Not a mountain (not inevitable—other democracies use different systems) but not pure extraction (real coordination benefit exists). d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.53.
constraint_indexing:constraint_classification(france_2027_presidential_election, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(france_2027_presidential_election_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(france_2027_presidential_election, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(france_2027_presidential_election, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(france_2027_presidential_election, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(france_2027_presidential_election, TR),
    TR >= 0.70.

:- end_tests(france_2027_presidential_election_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52, rising from 0.38): The Republican Barrier extracts from far-right voters at high rate (d≈0.92 → χ≈0.72 for that population) but provides genuine coordination benefit to centrists and moderate left (d≈0.08 and d≈0.78 respectively). The rising average extractiveness reflects that the distribution of costs is becoming more concentrated—more voters feel trapped, fewer experience genuine coordination benefit. Theater ratio (0.58, rising from 0.42): Increasing theatrical content indicates that mainstream party commitment to the Barrier is becoming performative. Elite signaling of Republican Barrier commitment continues, but private calculations about acceptable outcomes (some elites may privately prefer far-right victory to leftist victory) are increasingly visible. Suppression (0.65): Moderate-high. The two-round system suppresses authentic expression in round one; voters cannot vote their true preference without triggering constrained second-round outcomes. Publication bias in media (stories emphasizing Barrier necessity) suppresses alternative framings. However, suppression is not total—far-right voters do vote their preference in round one, accepting round-two capture as cost.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the widest perspectival gap of any mature democratic institution. Far-right voters experience a snare (trapped, no exit). Left-wing coalition partners experience snare with moderate escape routes (constrained exit). Centrist beneficiaries experience rope (functional coordination). The constitutional framework experiences tangled rope (both enabling and constraining). The Republican Barrier norm itself appears as piton (vestigial, maintained through theater). Electoral reform advocates see scaffold (temporary coordination requiring structural replacement). The analytical observer sees tangled rope (genuine coordination + real extraction). No two perspectives produce the same classification. This perspectival spectrum is not a measurement artifact—it reflects the genuine structural reality that the Barrier provides different functional roles to different agents. The same institution is Snare to the trapped, Rope to the beneficiaries, and Piton to the vestigial norm.
 *
 * DIRECTIONALITY LOGIC:
 *   Far-right voter base: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Centrist establishment: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary (negative extraction). Left coalition: Victim + constrained → d≈0.78, f(d)≈1.08. High extraction but not maximal (escape routes exist, albeit costly). Constitutional framework: Both beneficiary (enables governance) + victim (suppresses expression) + constrained exit → d≈0.50, f(d)≈0.65. Symmetric position. Republican Barrier norm: Institutional + arbitrage, but degrading → d≈0.08, f(d)≈-0.10 (derived), but piton gate (theater≥0.70... currently 0.58, approaching gate) shows norm is inertial. Electoral reform advocates: Powerful + mobile → d≈0.35, f(d)≈0.35. Low effective extraction because they perceive structural alternatives. Analytical observer: d≈0.68, f(d)≈1.02. Medium-high extraction from overall system perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   CORE MANDATROPHY: Does the Republican Barrier represent genuine coordination (necessary to prevent democratic backsliding and preserve constitutional governance) or extraction of democratic authenticity disguised as civic responsibility? The constraint resolves this by acknowledging both are simultaneously true. For centrist beneficiaries, it is coordination—the Barrier prevents far-right dominance and enables moderate governance. For far-right voters, it is pure extraction—their electoral will is systematically suppressed. For left-wing actors, it is tangled rope—they receive both coordination benefit (shared governance voice) and extraction cost (constrained negotiating power). The rising theater ratio and extractiveness suggest the coordination function is degrading relative to the extraction function. Elite commitment to the Barrier is increasingly performative—parties signal commitment while calculating private preference for outcomes outside the coalition. The constraint is not resolving mandatrophy by becoming pure extraction (the coordination function persists) but by becoming progressively less able to justify its extraction costs in coordination terms. If extractiveness reaches 0.65-0.70 while theater remains >0.60, the Barrier will have transitioned from tangled rope (mixed but defensible) to snare (extraction primarily, coordination incidental). Current trajectory (extractiveness rising, theater rising) suggests this threshold may be approached by 2032.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    barrier_threshold_collapse,
    'At what far-right vote share does the Republican Barrier cease functioning as coordination and become manifest coordination theater?',
    'Historical analysis of mainstream party defection rates; measurement of coalition compliance when far-right exceeds certain thresholds (currently 30-35%, trending upward); interviews with political elites on minimum acceptable election outcomes',
    'If threshold < 40%: barrier is already functionally degraded, more snare than rope. If threshold > 50%: barrier retains structural strength despite rhetoric. Current trajectory suggests threshold is 38-42%.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(barrier_threshold_collapse, empirical, 'Vote share threshold at which Republican Barrier transitions from coordination to theater').

omega_variable(
    coalition_cost_asymmetry,
    'Do centrist and left-wing leaders genuinely accept coalition terms, or are they performatively signaling while retaining hidden preferences for far-right victory?',
    'Post-election analysis of commitment depth: did coalition members campaign in contested districts? Did they negotiate concrete policy concessions? Did any major leader openly signal acceptance of far-right outcome as preferable to center-left unity?',
    'If genuine commitment: constraint is tangled rope (real coordination + real extraction). If performative: constraint is piton (theatrical commitment masking degraded consensus). Evidence from 2026 legislative elections will be instructive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_cost_asymmetry, empirical, 'Whether mainstream party coalition commitment is genuine or performative').

omega_variable(
    far_right_institutional_capacity,
    'Can the far-right effectively govern if it achieves presidency? Does institutional capacity constrain or enable their policy execution?',
    'Assessment of organizational depth, technocratic capacity, foreign policy experience, and institutional relationships. Comparative analysis with other right-populist governments (Hungary, Poland, Italy). Election outcome testing if far-right advances to second round.',
    'If low capacity: Barrier is functionally necessary (mountain-like coordination). If high capacity: Barrier is pure extraction (snare), suppressing legitimate electoral choice. Assessment suggests capacity is moderate-to-high, weakening mountain narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(far_right_institutional_capacity, empirical, 'Institutional capacity of far-right to govern if elected').

omega_variable(
    voter_preference_authenticity,
    'Do first-round voting patterns reflect authentic voter preferences, or are they strategic responses to anticipated Republican Barrier mechanics?',
    'Polling comparison of expressed preferences vs. voting intention; historical analysis of shift from round one to round two; counterfactual analysis of voting under proportional or ranked-choice systems',
    'If authentic: first-round results reveal genuine electorate demand for far-right. Barrier is extraction. If strategic: first-round results are distorted by system design. Barrier retains coordination legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voter_preference_authenticity, empirical, 'Whether first-round voting reflects authentic preference or strategic response to Barrier').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(france_2027_presidential_election, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fr27_tr_t0, france_2027_presidential_election, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fr27_tr_t6, france_2027_presidential_election, theater_ratio, 6, 0.52).
narrative_ontology:measurement(fr27_tr_t12, france_2027_presidential_election, theater_ratio, 12, 0.58).

% Extraction over time
narrative_ontology:measurement(fr27_be_t0, france_2027_presidential_election, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fr27_be_t6, france_2027_presidential_election, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(fr27_be_t12, france_2027_presidential_election, base_extractiveness, 12, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(france_2027_presidential_election, enforcement_mechanism).
narrative_ontology:affects_constraint(france_2027_presidential_election, eu_populism_backlash).
narrative_ontology:affects_constraint(france_2027_presidential_election, french_institutional_legitimacy).
narrative_ontology:affects_constraint(france_2027_presidential_election, european_democratic_resilience).

% DUAL FORMULATION NOTE:
% The Republican Barrier is downstream of broader European populism/anti-establishment dynamics but represents a distinct structural constraint at the national electoral level. The upstream constraints (EU legitimacy crisis, populist momentum) drive the Barrier's degradation; the Barrier's classification reflects the specific national institutional response.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(france_2027_presidential_election, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
