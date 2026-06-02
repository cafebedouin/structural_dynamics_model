% ============================================================================
% CONSTRAINT STORY: democratic_legitimacy_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_democratic_legitimacy_arbitrage, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: democratic_legitimacy_arbitrage
 *   human_readable: Democratic Legitimacy Arbitrage in Populist Governance
 *   domain: political_economy/comparative_politics/democratic_theory
 *
 * SUMMARY:
 *   The democratic legitimacy arbitrage emerges when populist supporters
 *   simultaneously endorse representative democracy as an abstract ideal and
 *   strong-leader governance without parliamentary or judicial interference
 *   as a practical necessity. This is not cognitive dissonance but a coherent
 *   redefinition of 'democracy' from institutional procedure (checks and
 *   balances, separation of powers, rule of law) to embodied popular will
 *   (the leader as direct representative of 'the people'). The arbitrage
 *   creates an extraction opportunity: populist leadership captures
 *   concentrated executive power while retaining democratic legitimacy
 *   credentials, eroding institutional checks and balances in the process.
 *   The constraint is downstream of post-industrial spatial extraction (which
 *   creates demand for governance that existing institutions cannot supply)
 *   and populist-as-class-realignment (which provides the electoral
 *   coalition). Survey data shows the structural contradiction: populist
 *   supporters rate both 'having a democratic political system' and 'having a
 *   strong leader who does not have to bother with parliament and elections'
 *   as highly desirable, while non-populist supporters show negative
 *   correlation between these preferences. The gap reveals the legitimacy
 *   arbitrage in action.
 *
 * KEY AGENTS:
 *   - Populist Supporter: Primary target (powerless/identity_locked) — identity constituted through the populist frame; cannot see the contradiction because 'democracy' has been redefined
 *   - Opposition Voter: Secondary victim (powerless/trapped) — sees the institutional erosion clearly but cannot exit the political system
 *   - Swing Voter: Mixed position (moderate/constrained) — benefits from simplified political choice, bears cost of institutional degradation
 *   - Populist Leadership: Primary beneficiary (institutional/arbitrage) — captures executive power while retaining democratic legitimacy
 *   - Judiciary: Institutional victim (institutional/constrained) — cannot exit constitutional role; faces sustained erosion of independence
 *   - International Democracy Monitors: Organized observers (organized/mobile) — can exit but also invested in the system; see both coordination and extraction
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies the legitimacy arbitrage as tangled rope (real coordination function, real extraction cost)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(democratic_legitimacy_arbitrage, 0.58).
domain_priors:suppression_score(democratic_legitimacy_arbitrage, 0.68).
domain_priors:theater_ratio(democratic_legitimacy_arbitrage, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(democratic_legitimacy_arbitrage, extractiveness, 0.58).
narrative_ontology:constraint_metric(democratic_legitimacy_arbitrage, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(democratic_legitimacy_arbitrage, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(democratic_legitimacy_arbitrage, snare).
narrative_ontology:human_readable(democratic_legitimacy_arbitrage, "Democratic Legitimacy Arbitrage in Populist Governance").
narrative_ontology:topic_domain(democratic_legitimacy_arbitrage, "political_economy/comparative_politics/democratic_theory").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(democratic_legitimacy_arbitrage, populist_leadership).
narrative_ontology:constraint_victim(democratic_legitimacy_arbitrage, institutional_checks_and_balances).
narrative_ontology:constraint_victim(democratic_legitimacy_arbitrage, opposition_political_actors).
narrative_ontology:constraint_victim(democratic_legitimacy_arbitrage, judicial_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POPULIST SUPPORTER (SNARE) — Identity-locked within the populist frame that treats democratic legitimacy as residing in the leader's direct connection to 'the people' rather than in institutional procedures. Simultaneously endorses representative democracy (as abstract ideal) and strong-leader governance without checks (as practical necessity). The contradiction is invisible from within the identity frame because 'democracy' has been redefined as popular will embodied in the leader. High extraction: institutional protections eroded, but supporter cannot exit because their political identity is constituted through the populist movement.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: OPPOSITION VOTER (SNARE) — Trapped by the electoral arithmetic and institutional erosion. Sees the contradiction clearly but cannot exit the political system. As checks and balances weaken, opposition electoral victories become structurally harder (gerrymandering, media capture, judicial interference). Maximum extraction: bears full cost of institutional degradation with no exit option.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: SWING VOTER (TANGLED ROPE) — Constrained by limited information and competing frames. Benefits from the coordination function (clear electoral choice, simplified political narrative) but also bears costs (institutional erosion affects everyone). Can exit the populist coalition at electoral cost (social pressure, information ecosystem lock-in) but not trapped. Mixed extraction: the legitimacy arbitrage creates a simpler political choice (coordination benefit) while degrading the institutional substrate (extraction cost).
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: POPULIST LEADERSHIP (ROPE) — Primary beneficiary with full arbitrage capacity. The legitimacy arbitrage is a coordination mechanism from this perspective: it resolves the tension between democratic legitimacy (requires popular mandate) and governance capacity (requires removing institutional obstacles). The leader experiences this as solving a genuine coordination problem — how to govern effectively while maintaining democratic credentials. Net beneficiary: captures concentrated executive power while retaining electoral legitimacy.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: JUDICIARY (SNARE) — Institutional victim with constrained exit. Can resist in the short term (ruling against executive overreach) but faces sustained pressure: court-packing threats, budget cuts, media attacks, non-compliance with rulings. Exit options are constrained: judges cannot leave the system without abandoning their institutional role, and the institution itself cannot exit the constitutional order. High extraction: judicial independence erodes as the legitimacy arbitrage redefines 'democratic' governance as unimpeded executive action.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL MONITORS (TANGLED ROPE) — Organized actors (OSCE, EU, NGOs) with mobile exit options (can withdraw monitoring, impose sanctions, shift resources). See both coordination (the populist frame does solve some governance deadlocks) and extraction (institutional erosion is real and measurable). Mixed experience: benefit from the clarity of the diagnostic (easier to measure democratic backsliding when the contradiction is explicit) but also bear costs (their monitoring legitimacy is challenged by the populist frame's redefinition of democracy).
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The legitimacy arbitrage is a genuine structural phenomenon, not a false summit. It coordinates populist supporters around a simplified political identity (coordination function) while extracting from institutional checks and balances (extraction function). The contradiction (endorsing both representative democracy and unchecked executive power) is not cognitive dissonance but a coherent redefinition of 'democracy' as popular will embodied in the leader rather than institutional procedure. This is tangled rope, not snare, from the analytical perspective because the coordination function is real: the arbitrage resolves a legitimacy crisis (post-industrial spatial extraction creates demand for governance that existing institutions cannot supply) even as it degrades those institutions.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(democratic_legitimacy_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(democratic_legitimacy_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(democratic_legitimacy_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The legitimacy arbitrage extracts from institutional checks and balances (judiciary, parliament, media, civil society) by redefining democratic legitimacy to exclude procedural constraints on executive power. The extraction is substantial but not total: some institutional resistance persists, and the populist leadership must maintain electoral legitimacy (cannot fully abandon democratic credentials). The value reflects that the arbitrage is a hybrid mechanism — it coordinates populist supporters around a simplified political identity while extracting from institutional constraints. Suppression (0.68): High. Significant barriers to exit for both populist supporters (identity lock) and opposition voters (electoral arithmetic, institutional erosion). The suppression requirement has increased over the interval as the legitimacy arbitrage has matured: early-stage populism allows more exit options (supporters can defect, opposition can win elections), but as institutions erode, exit becomes structurally harder. Theater ratio (0.64): Moderate-high. The democratic credentials are substantially performative: elections continue, but their function shifts from accountability mechanism to legitimacy ritual. Parliamentary oversight continues, but its function shifts from constraint to theater. The theater has increased over the interval as the gap between formal democratic procedure and actual governance has widened.
 *
 * PERSPECTIVAL GAP:
 *   The legitimacy arbitrage demonstrates how the same structural phenomenon appears differently depending on the observer's position and identity frame. Populist supporters see coordination (the leader solves governance problems) because their identity frame redefines democracy to exclude institutional constraints. Opposition voters see pure extraction (institutional erosion) because they retain the procedural definition of democracy. Swing voters see mixed coordination and extraction (simplified choice, degraded institutions). Populist leadership sees coordination (effective governance) because they are the beneficiary. The judiciary sees extraction (independence eroded) because they are the institutional victim. International monitors see tangled rope (both functions are real). The analytical observer sees tangled rope because the coordination function is genuine (the arbitrage does resolve a legitimacy crisis created by post-industrial spatial extraction) even as the extraction is real (institutions degrade). The perspectival gap is not about who is correct but about which structural features are visible from each position.
 *
 * DIRECTIONALITY LOGIC:
 *   Populist supporters are identity-locked victims: their identity is constituted through the populist frame, which redefines democracy as embodied popular will rather than institutional procedure. This produces high d (victim + identity_locked) → high f(d) → high chi, but the supporter cannot perceive the extraction because the frame makes the contradiction invisible. Opposition voters are trapped victims: they see the extraction clearly but cannot exit. Swing voters are constrained mixed actors: they benefit from the coordination function (simplified choice) but bear extraction costs (institutional erosion affects everyone). Populist leadership is the primary beneficiary with arbitrage exit: they capture executive power while retaining democratic legitimacy, experiencing low d → low/negative chi. The judiciary is an institutional victim with constrained exit: cannot abandon constitutional role, faces sustained erosion. International monitors are organized actors with mobile exit: can withdraw resources but also invested in the system. The analytical observer sees tangled rope: real coordination function (resolves legitimacy crisis) and real extraction (institutional degradation).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that the legitimacy arbitrage is tangled rope from the analytical perspective (genuine coordination function + genuine extraction) but appears as snare from multiple victim perspectives (populist supporters who are identity-locked, opposition voters who are trapped, judiciary that is institutionally constrained). The mandatrophy question 'Is this coordination or extraction?' has the answer 'Both, and the ratio depends on the observer's structural position.' The populist leadership experiences it as coordination (rope) because they are the beneficiary. The victims experience it as extraction (snare) because they bear the costs. The analytical observer sees both functions operating simultaneously (tangled rope). The classification is not arbitrary — it follows from the structural data (beneficiary/victim declarations, exit options, power levels) and the indexical tuple.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_redefinition_threshold,
    'At what point does the populist redefinition of ''democracy'' (from institutional procedure to embodied popular will) become the dominant frame within the polity?',
    'Longitudinal survey data tracking definitions of democracy; institutional compliance rates with judicial rulings; media framing analysis of democratic legitimacy',
    'If threshold is crossed: the constraint becomes self-reinforcing (the redefined frame is the new normal). If threshold is not crossed: the arbitrage remains contested and potentially reversible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_redefinition_threshold, empirical, 'Threshold for legitimacy frame dominance').

omega_variable(
    institutional_resilience_floor,
    'Do institutional checks and balances have a resilience floor below which they cannot be further eroded without triggering regime collapse or external intervention?',
    'Comparative analysis of democratic backsliding cases; identification of institutional collapse thresholds (military intervention, constitutional crisis, international sanctions)',
    'If floor exists: extraction is bounded (snare cannot degrade to full autocracy without triggering collapse). If no floor: extraction is unbounded (gradual transition to autocracy is possible).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_resilience_floor, empirical, 'Whether institutional erosion has a collapse threshold').

omega_variable(
    coordination_function_necessity,
    'Is the legitimacy arbitrage a necessary response to genuine governance failures (post-industrial spatial extraction, institutional sclerosis), or is it an opportunistic extraction mechanism that exploits those failures?',
    'Counterfactual analysis: do polities with similar governance failures but different institutional responses (e.g., technocratic reform, coalition government) achieve comparable governance outcomes without institutional erosion?',
    'If necessary: the constraint is tangled rope from more perspectives (genuine coordination function). If opportunistic: the constraint is snare from more perspectives (extraction masquerading as coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_necessity, conceptual, 'Whether the arbitrage solves a real coordination problem or exploits a crisis').

omega_variable(
    identity_lock_reversibility,
    'Can populist supporters exit the identity lock (the redefined frame of democracy as embodied popular will) at biographical timescales, or is the identity fusion permanent within a generation?',
    'Panel survey data tracking individual-level shifts in democratic attitudes; analysis of populist coalition defection rates; post-populist political identity formation',
    'If reversible: the identity lock is a temporary cognitive capture (exit possible with frame-breaking events). If permanent: the identity lock is generational (exit requires cohort replacement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether identity-locked supporters can exit within biographical time').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(democratic_legitimacy_arbitrage, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dem_arb_tr_t0, democratic_legitimacy_arbitrage, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dem_arb_tr_t3, democratic_legitimacy_arbitrage, theater_ratio, 3, 0.53).
narrative_ontology:measurement(dem_arb_tr_t6, democratic_legitimacy_arbitrage, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(dem_arb_be_t0, democratic_legitimacy_arbitrage, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(dem_arb_be_t3, democratic_legitimacy_arbitrage, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(dem_arb_be_t6, democratic_legitimacy_arbitrage, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(dem_arb_su_t0, democratic_legitimacy_arbitrage, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(dem_arb_su_t3, democratic_legitimacy_arbitrage, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(dem_arb_su_t6, democratic_legitimacy_arbitrage, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(democratic_legitimacy_arbitrage, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of post_industrial_spatial_extraction (mountain — creates demand for governance that existing institutions cannot supply) and populist_as_class_realignment (tangled_rope — provides the electoral coalition). The legitimacy arbitrage is the mechanism by which the populist coalition captures executive power while retaining democratic legitimacy credentials. The upstream constraints have their own extractiveness values; this constraint has its own extractiveness reflecting the institutional erosion and identity lock created by the arbitrage mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
