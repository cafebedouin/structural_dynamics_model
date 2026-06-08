% ============================================================================
% CONSTRAINT STORY: legitimacy_narrative_inversion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_narrative_inversion, []).

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
 *   constraint_id: legitimacy_narrative_inversion
 *   human_readable: Putin Regime Legitimacy Narrative Inversion
 *   domain: political_economy/regime_stability/military_conflict
 *
 * SUMMARY:
 *   The Putin regime's core legitimacy claim — that Putin rescued Russia from
 *   the chaos, humiliation, and economic collapse of the 1990s — is inverting
 *   as the current trajectory reproduces 1990s conditions: deposit freezes,
 *   savings certificate production, business closure rates matching 1990s
 *   patterns, and elite predictions of 1917/1989-style collapse. This
 *   constraint is a piton: the legitimacy narrative has atrophied from a
 *   functional coordination mechanism (early Putin era: the claim stabilized
 *   expectations and coordinated elite behavior) into a degraded ritual
 *   maintained theatrically. The regime continues to assert the stability
 *   narrative even as observable conditions contradict it, because no
 *   alternative legitimacy framework has replaced it and because the
 *   performative maintenance buys time for elite extraction and exit
 *   preparation. The theater ratio (0.78) reflects that the narrative is now
 *   primarily performance rather than function. The constraint is downstream
 *   of the deathonomics collapse (the economic crisis driving the
 *   1990s-pattern reproduction) but represents a distinct structural
 *   phenomenon: the legitimacy claim's loss of functional content.
 *
 * KEY AGENTS:
 *   - Russian Population: Primary victim (powerless/trapped) — bears full cost of economic collapse while regime maintains theatrical stability narrative; cannot exit
 *   - Regional Business Owners: Secondary victim (moderate/constrained) — face business closures and economic chaos while regime suppresses acknowledgment of crisis; constrained exit due to capital controls and asset seizure risk
 *   - State Propaganda Apparatus: Mixed position (institutional/constrained) — benefits from employment and status but bears reputational cost of maintaining implausible narrative; constrained exit due to career and safety dependence on regime
 *   - Regime Inner Circle: Primary beneficiary (institutional/arbitrage) — uses narrative to coordinate elite behavior and suppress dissent during immediate crisis; arbitrage-grade exit options (offshore assets, foreign residency)
 *   - Opposition Coalition: Latent organized agents (organized/mobile) — currently suppressed but structurally present as potential alternative legitimacy framework; would experience narrative collapse as scaffold enabling transition
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the constraint as a piton, a degraded ritual maintained through inertia and performance rather than function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_narrative_inversion, 0.28).
domain_priors:suppression_score(legitimacy_narrative_inversion, 0.62).
domain_priors:theater_ratio(legitimacy_narrative_inversion, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_narrative_inversion, extractiveness, 0.28).
narrative_ontology:constraint_metric(legitimacy_narrative_inversion, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(legitimacy_narrative_inversion, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_narrative_inversion, piton).
narrative_ontology:human_readable(legitimacy_narrative_inversion, "Putin Regime Legitimacy Narrative Inversion").
narrative_ontology:topic_domain(legitimacy_narrative_inversion, "political_economy/regime_stability/military_conflict").

domain_priors:requires_active_enforcement(legitimacy_narrative_inversion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_narrative_inversion, regime_inner_circle).
narrative_ontology:constraint_beneficiary(legitimacy_narrative_inversion, state_propaganda_apparatus).
narrative_ontology:constraint_victim(legitimacy_narrative_inversion, russian_population).
narrative_ontology:constraint_victim(legitimacy_narrative_inversion, regional_business_owners).
narrative_ontology:constraint_victim(legitimacy_narrative_inversion, deposit_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RUSSIAN POPULATION (PITON) — Trapped within national borders with biographical stakes in regime stability. The legitimacy narrative ('Putin rescued Russia from 1990s chaos') has atrophied into pure performance: the regime maintains the claim theatrically even as current conditions reproduce 1990s dysfunction (deposit freezes, business closures, economic chaos). The population experiences the constraint as a degraded ritual — the narrative no longer functions to stabilize expectations or coordinate behavior, but persists because no alternative legitimacy framework has replaced it. High theater ratio reflects that the claim is maintained performatively rather than functionally.
constraint_indexing:constraint_classification(legitimacy_narrative_inversion, piton,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL BUSINESS OWNERS (SNARE) — Constrained exit (capital controls, asset seizure risk, family ties) but not fully trapped. Experience the legitimacy inversion as pure extraction: the regime's narrative claim ('stability and prosperity') actively suppresses acknowledgment of economic collapse, preventing coordination on exit strategies or collective action. Business closure rates match 1990s patterns, but the regime's performative maintenance of the stability narrative blocks political space for addressing the crisis. The coordination story (regime provides stability) is cover; persistence depends on suppression of alternatives.
constraint_indexing:constraint_classification(legitimacy_narrative_inversion, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE PROPAGANDA APPARATUS (TANGLED ROPE) — Institutional actors with constrained exit (career and physical safety depend on regime continuity). Experience genuine coordination function (the apparatus coordinates messaging across state media, regional governments, and proxy channels) alongside asymmetric extraction (the narrative inversion creates cognitive dissonance and reputational damage as the gap between claim and reality widens). The apparatus benefits from employment and status but bears the cost of maintaining an increasingly implausible narrative. Active enforcement required to suppress alternative framings.
constraint_indexing:constraint_classification(legitimacy_narrative_inversion, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REGIME INNER CIRCLE (ROPE) — Institutional power with arbitrage-grade exit (offshore assets, foreign residency options, exit plans). Experience the legitimacy narrative as pure coordination: the claim ('Putin rescued Russia') coordinates elite behavior and suppresses internal dissent during the immediate crisis. The inner circle are net beneficiaries — the narrative's performative maintenance buys time for asset extraction and exit preparation. Low effective extraction because this group has agency and benefits from the constraint's operation.
constraint_indexing:constraint_classification(legitimacy_narrative_inversion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: OPPOSITION COALITION (SCAFFOLD) — Organized agents (currently suppressed but structurally present as latent coalition) with mobile exit options (emigration, underground networks). Would experience the legitimacy inversion as a temporary coordination problem with implicit sunset: the narrative's collapse creates political space for alternative legitimacy frameworks. The constraint is transitional — its justification is the transition from Putin-era stability claim to post-Putin governance, not the steady state. This perspective is counterfactual (the coalition is suppressed) but structurally coherent: if the regime falls, the legitimacy inversion becomes the scaffold that enabled the transition.
constraint_indexing:constraint_classification(legitimacy_narrative_inversion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (PITON) — From a civilizational/global perspective, the legitimacy narrative inversion is a degraded institutional ritual. The regime's core claim ('rescued Russia from 1990s chaos') has lost its functional content as current conditions reproduce 1990s dysfunction, but the claim persists through institutional inertia and theatrical maintenance. The analytical observer sees what the trapped population experiences: a piton — a constraint whose primary function has atrophied but remains due to performance rather than coordination or extraction. The theater ratio is the diagnostic signal.
constraint_indexing:constraint_classification(legitimacy_narrative_inversion, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_narrative_inversion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legitimacy_narrative_inversion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legitimacy_narrative_inversion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(legitimacy_narrative_inversion, TR),
    TR >= 0.70.

:- end_tests(legitimacy_narrative_inversion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate and declining. The legitimacy narrative's extraction has decreased over the interval as its functional content atrophied. Early in the interval (2014), the narrative extracted more — it actively coordinated behavior and suppressed alternatives, benefiting the regime at the population's expense. By 2025, the narrative is primarily theatrical: the regime maintains the claim performatively, but the claim no longer functions to stabilize expectations or coordinate behavior. The extraction that remains is the opportunity cost of the suppressed alternative (acknowledgment of crisis and coordination on solutions). The declining trajectory reflects the piton dynamic: as function atrophies, extraction decreases, but theater increases. Suppression (0.62): Moderate-high and rising. The regime's suppression of alternative narratives has intensified as the gap between claim and reality widened. Capital controls, media censorship, political repression, and exit barriers all increased over the interval. The rising trajectory reflects that maintaining the theatrical narrative requires increasing coercion as its implausibility grows. Theater ratio (0.78): High and rising steeply. The legitimacy narrative was functional in 2014 (theater ratio 0.35) — it genuinely coordinated elite behavior and stabilized expectations after Crimea annexation. By 2025, the narrative is primarily performance: the regime asserts stability while conditions reproduce 1990s chaos. The steep rise in theater ratio is the diagnostic signal of piton classification: the constraint's primary function has atrophied, but the constraint persists through institutional inertia and performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a clear perspectival gap driven by power and exit differentials. The trapped population experiences a piton — a degraded ritual that no longer functions but persists through inertia. Regional business owners experience a snare — the narrative actively suppresses coordination on exit or collective action. The state propaganda apparatus experiences tangled rope — genuine coordination function (messaging coordination) alongside extraction (reputational damage from maintaining implausible claims). The regime inner circle experiences rope — the narrative coordinates elite behavior and buys time for extraction. The latent opposition coalition would experience scaffold — the narrative's collapse creates political space for transition. The analytical observer confirms the piton classification: the constraint's primary function has atrophied, but it persists through theatrical maintenance. The gap between the population's piton experience and the inner circle's rope experience reveals the constraint's asymmetric structure: what is a degraded ritual for those trapped within it is a functional coordination mechanism for those with exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The Russian population is the primary victim (trapped, powerless) — they bear the full cost of economic collapse while the regime maintains the theatrical stability narrative. Regional business owners are secondary victims (constrained, moderate) — they face business closures and suppression of crisis acknowledgment, with constrained but not impossible exit. The state propaganda apparatus has a mixed position (constrained, institutional) — they benefit from employment but bear reputational cost; their directionality is moderate because they experience both coordination (messaging coordination) and extraction (cognitive dissonance, reputational damage). The regime inner circle are primary beneficiaries (arbitrage, institutional) — they use the narrative to coordinate elite behavior and buy time for asset extraction; their directionality is low because they are net beneficiaries with exit options. The opposition coalition (mobile, organized) would experience low directionality if they were active — they would see the narrative collapse as enabling transition. The analytical observer (analytical, analytical) has no directionality — they observe the structure without being subject to it.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved: the legitimacy narrative's mandate (rescue Russia from 1990s chaos) has outlived its function as current conditions reproduce 1990s dysfunction. The constraint is maintained theatrically rather than functionally. The piton classification captures this: the narrative persists through institutional inertia and performative maintenance, not because it coordinates behavior or extracts rents effectively. The mandate-function gap is the structural signature of mandatrophy resolution. The regime continues to assert the stability claim even as observable conditions contradict it, because no alternative legitimacy framework has replaced it and because the performative maintenance buys time for elite extraction and exit preparation. The theater ratio (0.78) is the quantitative measure of the mandate-function gap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    narrative_collapse_threshold,
    'At what threshold of 1990s-pattern reproduction does the legitimacy narrative lose all functional content and become purely theatrical?',
    'Longitudinal tracking of public discourse: ratio of 1990s comparisons to stability claims; correlation between economic indicators (deposit freezes, business closures, savings certificate production) and narrative maintenance effort (state media frequency, suppression intensity)',
    'If threshold already crossed: piton classification confirmed — the narrative is maintained performatively. If threshold not yet reached: the constraint retains some coordination function and may classify as tangled_rope from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrative_collapse_threshold, empirical, 'Threshold at which legitimacy narrative loses functional content').

omega_variable(
    elite_exit_timing,
    'Do regime elites with arbitrage-grade exit options perceive the legitimacy inversion as a coordination mechanism (buying time for exit) or as a genuine stability claim?',
    'Analysis of elite behavior: capital flight patterns, asset liquidation timing, family relocation, offshore account activity. If elites are exiting while maintaining the narrative, the coordination function is real but cynical.',
    'If elites are exiting: rope classification from inner circle perspective confirmed — the narrative coordinates elite extraction. If elites are not exiting: the narrative may retain genuine belief among beneficiaries, complicating the piton diagnosis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(elite_exit_timing, empirical, 'Whether elite exit behavior reveals cynical coordination or genuine belief').

omega_variable(
    opposition_coalition_latency,
    'Is the opposition coalition structurally present but suppressed, or is it genuinely absent due to atomization and exit?',
    'Network analysis of dissident activity, emigration patterns, underground organizing. If networks exist but are suppressed, the scaffold perspective is structurally real. If atomization is complete, the scaffold perspective is aspirational.',
    'If coalition is latent: scaffold perspective valid — the legitimacy inversion creates political space for transition. If coalition is absent: no organized agent sees a sunset, and the constraint persists as piton or snare indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(opposition_coalition_latency, empirical, 'Whether opposition coalition is suppressed or absent').

omega_variable(
    historical_parallel_validity,
    'Are 1917 and 1989 collapse predictions structurally analogous to current conditions, or are they performative historical framing?',
    'Comparative historical analysis: economic indicators, elite cohesion, military capacity, external pressure, popular mobilization potential. Structural comparison of pre-collapse conditions across 1917, 1989, and present.',
    'If structurally analogous: the legitimacy inversion is a leading indicator of regime collapse, and the piton classification captures a terminal-phase constraint. If not analogous: the historical framing is itself theatrical, and the constraint may persist longer than the collapse predictions suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_parallel_validity, conceptual, 'Whether historical collapse analogies are structurally valid or performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_narrative_inversion, 0, 11).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legit_inv_theater_2014, legitimacy_narrative_inversion, theater_ratio, 0, 0.35).
narrative_ontology:measurement(legit_inv_theater_2017, legitimacy_narrative_inversion, theater_ratio, 3, 0.48).
narrative_ontology:measurement(legit_inv_theater_2020, legitimacy_narrative_inversion, theater_ratio, 6, 0.62).
narrative_ontology:measurement(legit_inv_theater_2023, legitimacy_narrative_inversion, theater_ratio, 9, 0.73).
narrative_ontology:measurement(legit_inv_theater_2025, legitimacy_narrative_inversion, theater_ratio, 11, 0.78).

% Extraction over time
narrative_ontology:measurement(legit_inv_extract_2014, legitimacy_narrative_inversion, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(legit_inv_extract_2017, legitimacy_narrative_inversion, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(legit_inv_extract_2020, legitimacy_narrative_inversion, base_extractiveness, 6, 0.33).
narrative_ontology:measurement(legit_inv_extract_2023, legitimacy_narrative_inversion, base_extractiveness, 9, 0.3).
narrative_ontology:measurement(legit_inv_extract_2025, legitimacy_narrative_inversion, base_extractiveness, 11, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(legit_inv_suppress_2014, legitimacy_narrative_inversion, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(legit_inv_suppress_2017, legitimacy_narrative_inversion, suppression_requirement, 3, 0.52).
narrative_ontology:measurement(legit_inv_suppress_2020, legitimacy_narrative_inversion, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(legit_inv_suppress_2023, legitimacy_narrative_inversion, suppression_requirement, 9, 0.61).
narrative_ontology:measurement(legit_inv_suppress_2025, legitimacy_narrative_inversion, suppression_requirement, 11, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_narrative_inversion, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of deathonomics_collapse (the economic crisis driving 1990s-pattern reproduction) but represents a distinct structural phenomenon: the legitimacy claim's loss of functional content. The upstream constraint (deathonomics_collapse) is a snare with high extraction; this constraint (legitimacy_narrative_inversion) is a piton with low extraction and high theater. The two constraints are linked but have different ε values and different structural dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_narrative_inversion, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
