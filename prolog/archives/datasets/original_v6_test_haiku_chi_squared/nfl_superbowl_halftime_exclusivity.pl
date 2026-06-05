% ============================================================================
% CONSTRAINT STORY: nfl_superbowl_halftime_exclusivity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nfl_superbowl_halftime_exclusivity, []).

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
 *   constraint_id: nfl_superbowl_halftime_exclusivity
 *   human_readable: NFL Super Bowl Halftime Show Exclusivity Agreements
 *   domain: economic/sports_entertainment
 *
 * SUMMARY:
 *   The NFL Super Bowl Halftime Show represents one of the most valuable
 *   entertainment platforms globally—approximately 115 million viewers for a
 *   12-minute performance. The NFL leverages this dominance to negotiate
 *   exclusivity agreements with selected performers, restricting their
 *   ability to perform similar content on competing platforms during a
 *   contractually defined window (typically 1-2 years). This constraint
 *   exemplifies extraction wrapped in prestige: the performer receives global
 *   exposure but surrenders creative autonomy, licensing rights, and platform
 *   optionality. The constraint exhibits perspectival pluralism: the selected
 *   performer (powerless/trapped) experiences it as a Snare; the NFL
 *   (institutional/arbitrage) experiences it as Rope; competing platforms
 *   experience it as Snare; superstar performers with pre-existing power
 *   experience it as Tangled Rope with negotiating leverage. The constraint
 *   has intensified over 30 years as the Super Bowl's cultural dominance has
 *   grown and streaming platforms have emerged as competitors.
 *
 * KEY AGENTS:
 *   - NFL League: Primary beneficiary (institutional/arbitrage) — captures exclusive rights to performer during peak relevance; controls narrative around halftime content; monetizes performer's reach
 *   - Selected Performer: Primary victim (powerless/trapped) — faces binary choice: accept prestige trap or reject career-defining opportunity; surrenders creative autonomy and platform options
 *   - Competing Platforms: Secondary victim (moderate/constrained) — streaming services, concert promoters, music festivals lose access to performer during exclusivity window; cannot compete on audience guarantee
 *   - Performer Compensation Market: Victim (structural) — exclusivity agreements suppress market price discovery; performers accept below-market compensation in exchange for prestige rather than negotiating from equal footing
 *   - Major Artists / Superstars: Mobile beneficiary (powerful/mobile) — A-list performers can negotiate carve-outs, demand equity, or decline; exclusivity is minor constraint relative to prestige value; have exit options
 *   - Music Industry Guild / Unions: Weak advocate (organized/constrained) — nominally protect performer interests but limited enforcement power; negotiation theater without substantive protection for trapped performers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nfl_superbowl_halftime_exclusivity, 0.58).
domain_priors:suppression_score(nfl_superbowl_halftime_exclusivity, 0.65).
domain_priors:theater_ratio(nfl_superbowl_halftime_exclusivity, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nfl_superbowl_halftime_exclusivity, extractiveness, 0.58).
narrative_ontology:constraint_metric(nfl_superbowl_halftime_exclusivity, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(nfl_superbowl_halftime_exclusivity, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nfl_superbowl_halftime_exclusivity, snare).
narrative_ontology:human_readable(nfl_superbowl_halftime_exclusivity, "NFL Super Bowl Halftime Show Exclusivity Agreements").
narrative_ontology:topic_domain(nfl_superbowl_halftime_exclusivity, "economic/sports_entertainment").

domain_priors:requires_active_enforcement(nfl_superbowl_halftime_exclusivity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nfl_superbowl_halftime_exclusivity, nfl_league).
narrative_ontology:constraint_victim(nfl_superbowl_halftime_exclusivity, performer_creative_autonomy).
narrative_ontology:constraint_victim(nfl_superbowl_halftime_exclusivity, competing_platforms).
narrative_ontology:constraint_victim(nfl_superbowl_halftime_exclusivity, performer_compensation_market).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SELECTED PERFORMER (SNARE) — Career-defining exposure creates a trap. Rejecting the halftime slot is career suicide; accepting requires surrendering creative autonomy, licensing rights, and future platform options under exclusivity. Trapped by prestige asymmetry. d≈0.92, f(d)≈1.39, σ=1.2 → χ≈0.79.
constraint_indexing:constraint_classification(nfl_superbowl_halftime_exclusivity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE PLATFORM (SNARE) — Competing entertainment platforms (Netflix, concert promoters, music festivals) lose access to selected performer for 1-2 years during peak relevance. Cannot outbid NFL for exclusivity. Constrained by asymmetric bargaining power. d≈0.78, f(d)≈1.18, σ=1.0 → χ≈0.68.
constraint_indexing:constraint_classification(nfl_superbowl_halftime_exclusivity, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE NFL LEAGUE (ROPE) — Experiences the exclusivity agreement as coordination: controlling halftime content protects broadcast quality, avoids competing media narratives, and maximizes viewer attention. Coordinates performer availability. Benefits from exclusive rights to performer's promotion during peak relevance window. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.05. Net beneficiary.
constraint_indexing:constraint_classification(nfl_superbowl_halftime_exclusivity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE MAJOR ARTIST / SUPERSTAR (TANGLED ROPE) — A-list performers with pre-existing global platform (Beyoncé, The Weeknd, Rihanna tier) have mobility and arbitrage options. Can negotiate carve-outs, demand compensation, or decline. Exclusivity is coordinating mechanism (global reach, cultural moment) combined with minor extraction (limited exclusivity scope for established artists). d≈0.35, f(d)≈0.25, σ=1.2 → χ≈0.17.
constraint_indexing:constraint_classification(nfl_superbowl_halftime_exclusivity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: THE MUSIC INDUSTRY GUILD / UNION (PITON) — Nominally advocates for performer rights but has limited enforcement power against NFL prestige capture. Theater_ratio=0.62: organizations stage negotiations, issue statements, and propose 'fair compensation frameworks,' but the performer trap persists. The mechanism (artist association advocacy) is largely performative because no individual performer can risk defection.
constraint_indexing:constraint_classification(nfl_superbowl_halftime_exclusivity, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE SUMMIT RISK) — From a universal/civilizational perspective, one might frame this as an immutable feature of market dominance: the largest platform always captures performers. However, the base properties (ε=0.58, suppression=0.65, theater=0.48) contradict a mountain classification. The constraint is contingent on NFL's current market dominance and exclusivity practices, not on inherent law. Engine will flag false summit.
constraint_indexing:constraint_classification(nfl_superbowl_halftime_exclusivity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nfl_superbowl_halftime_exclusivity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nfl_superbowl_halftime_exclusivity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nfl_superbowl_halftime_exclusivity, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nfl_superbowl_halftime_exclusivity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nfl_superbowl_halftime_exclusivity, TR),
    TR >= 0.70.

:- end_tests(nfl_superbowl_halftime_exclusivity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High and rising. The NFL extracts significant value from performer exclusivity by controlling global media narrative during peak relevance window, restricting alternative platform access, and suppressing market-based compensation negotiation. The trajectory from 0.35→0.48→0.58 over 30 years reflects increasing asymmetry as streaming platforms emerged as competitors and the Super Bowl's cultural value concentrated. The extraction is not total (0.70+) because some performers retain negotiating power and the Super Bowl's prestige does provide genuine career benefits alongside the extraction. Suppression (0.65): Moderate-high. The performer trap operates through prestige asymmetry, not physical coercion — but the alternatives are constrained: rejecting the opportunity has career costs (alternative platforms cannot offer equivalent reach), and accepting requires surrendering autonomy. Theater ratio (0.48): Moderate. The mechanism is relatively transparent — contracts explicitly state exclusivity terms — but there is performative layering: negotiation theater staged by guilds, media coverage of 'big payday' that obscures compressed compensation relative to the performer's value, and NFL messaging that frames exclusivity as mutual benefit.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the selected performer's perspective (Snare) and the NFL's perspective (Rope) is maximal. For the performer, the constraint is coercive: accept prestige and loss of autonomy or reject and forfeit career opportunity. For the NFL, the constraint is coordinating: it ensures content quality, prevents competing narratives, and maximizes broadcast value. Superstar performers occupy a bridge perspective (Tangled Rope): they have genuine options and can negotiate, but the exclusivity remains partially extractive because their negotiating power does not fully offset the NFL's advantage. Competing platforms see pure Snare — they have no leverage and cannot participate. The falsehood of the mountain perspective reveals how market dominance can be mistaken for natural law: the NFL's exclusivity does not flow from inherent scarcity (performers exist; platforms exist) but from concentrated bargaining power.
 *
 * DIRECTIONALITY LOGIC:
 *   Selected performer (powerless + trapped): d≈0.92, f(d)≈1.39. Binary choice with career cost to refusal creates maximum d. Alternative platform (moderate + constrained): d≈0.78, f(d)≈1.18. Can exit but with significant cost; no countervailing power. NFL league (institutional + arbitrage): d≈0.08, f(d)≈-0.08. Beneficiary with abundant exit options and market power. Superstar (powerful + mobile): d≈0.35, f(d)≈0.25. Mobile enough to negotiate; still partially trapped by prestige asymmetry. Music guild (organized + constrained): d≈0.58, f(d)≈0.85. Constrained advocacy; cannot overcome individual performer's incentive to accept.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy because the perspectival plurality is explicable by structural position, not by measurement ambiguity. The same exclusivity mechanism is genuinely coordinating from the NFL's perspective (they achieve orderly broadcast control) and genuinely extractive from the performer's perspective (suppressed compensation, lost optionality). The separation of perspectives by power and exit options explains the gap—no single measurement of 'true' classification. The potential mandatrophy lies in the mountain perspective: one might claim that 'prestige asymmetry is an inherent feature of any one-time mega-event, therefore the constraint is natural law.' This is false. Alternative mega-events (World Cup opening ceremony, Olympics opening ceremony) do not impose comparable exclusivity; the NFL's dominance is institutional choice, not structural necessity. The false summit is caught by noting that the base properties (ε=0.58, suppression=0.65, theater=0.48) do not meet mountain thresholds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    superstar_threshold_effect,
    'At what level of pre-existing fame do performers transition from Snare (powerless) to Tangled Rope (powerful)? Is it categorical or continuous?',
    'Comparative analysis of negotiation outcomes: compensation, exclusivity duration, creative control, carve-out clauses as function of performer''s prior Spotify streams, ticket sales, or social media reach',
    'If threshold is sharp: enables predictive classification of future performers. If continuous: classification depends on trajectory, not fixed properties. If threshold has risen over time: indicates market saturation effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(superstar_threshold_effect, empirical, 'Performer fame threshold distinguishing powerless from powerful perspectives').

omega_variable(
    exclusivity_enforcement_mechanism,
    'How does the NFL enforce exclusivity? What are the actual contractual penalties, and have they ever been invoked against non-compliance?',
    'Analysis of signed Super Bowl halftime performer contracts (to extent publicly available); research on any public disputes or attempted violations; comparison of stated vs de facto enforcement',
    'If enforcement is weak/unenforced: suppression is lower, classification shifts toward Rope. If enforcement is strict: suppression is higher, confirms Snare. If enforcement is selective (enforced against powerless, waived for powerful): confirms directionality asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusivity_enforcement_mechanism, empirical, 'Actual enforcement of exclusivity clauses').

omega_variable(
    alternative_broadcast_legitimacy,
    'If a performer performed the same content on a competing platform (Netflix special, concert film) during the exclusivity window but before the Super Bowl aired, would the NFL have a legitimate claim of harm, or is the claim purely about broadcast priority?',
    'Analysis of contract language: does exclusivity protect pre-Super Bowl broadcast rights, post-Super Bowl performance rights, or both? Historical cases where performers tested boundaries.',
    'If exclusivity is purely about Super Bowl broadcast: constraint is weaker (coordination-heavy). If exclusivity extends to future performance rights: constraint is stronger (extraction-heavy). If unclear in contracts: theater ratio increases (performative enforcement).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_broadcast_legitimacy, conceptual, 'Scope and timing of exclusivity enforceability').

omega_variable(
    nfl_market_dominance_dependency,
    'Would the NFL''s exclusivity extraction survive if competing platforms (streaming services, premium concert networks) were to offer equivalent or greater audience guarantees?',
    'Counterfactual historical analysis: cases where performers have turned down Super Bowl for competing mega-events. Comparison with other one-time mega-events (World Cup opening ceremony, Olympics opening ceremony) and their exclusivity practices.',
    'If extraction survives competition: constraint is structural (true Snare). If extraction collapses: constraint is dependent on NFL''s monopoly position (Snare that degrades if market changes).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nfl_market_dominance_dependency, empirical, 'Whether exclusivity is sustainable without NFL''s market dominance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nfl_superbowl_halftime_exclusivity, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nflhalf_tr_t0, nfl_superbowl_halftime_exclusivity, theater_ratio, 0, 0.32).
narrative_ontology:measurement(nflhalf_tr_t15, nfl_superbowl_halftime_exclusivity, theater_ratio, 15, 0.4).
narrative_ontology:measurement(nflhalf_tr_t30, nfl_superbowl_halftime_exclusivity, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(nflhalf_be_t0, nfl_superbowl_halftime_exclusivity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nflhalf_be_t15, nfl_superbowl_halftime_exclusivity, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(nflhalf_be_t30, nfl_superbowl_halftime_exclusivity, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nfl_superbowl_halftime_exclusivity, enforcement_mechanism).
narrative_ontology:affects_constraint(nfl_superbowl_halftime_exclusivity, nfl_broadcast_monopoly).
narrative_ontology:affects_constraint(nfl_superbowl_halftime_exclusivity, sports_performer_compensation_asymmetry).

% DUAL FORMULATION NOTE:
% The halftime exclusivity agreement is downstream of the NFL's broader broadcast monopoly and upstream of performer compensation asymmetry in sports entertainment. Each story has distinct ε: the broadcast monopoly may be structural (higher ε, mountain risk), while performer compensation asymmetry is sectoral (distinct victims, different measurement basis).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
