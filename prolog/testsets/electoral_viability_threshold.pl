% ============================================================================
% CONSTRAINT STORY: electoral_viability_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electoral_viability_threshold, []).

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
 *   constraint_id: electoral_viability_threshold
 *   human_readable: Electoral Viability Threshold
 *   domain: political_economy/electoral_systems
 *
 * SUMMARY:
 *   The electoral viability threshold is a structural constraint that
 *   prevents candidates and parties from appearing on ballots, participating
 *   in debates, receiving media coverage, and accessing donor networks unless
 *   they demonstrate prior electoral success. This creates a catch-22:
 *   candidates need resources and visibility to achieve viability, but only
 *   achieve resources and visibility after demonstrating viability. The
 *   constraint exhibits characteristics of both coordination (two-party
 *   systems reduce transaction costs and provide clear signals) and
 *   extraction (the same mechanism suppresses policy alternatives and voter
 *   choice). The viability threshold operates through multiple enforcement
 *   mechanisms: ballot-access signatures and filing fees (40+ states require
 *   5%-15% of prior vote), debate commission polling thresholds (15% to
 *   qualify for major debates), media narrative (viability framing in polls
 *   and coverage), and donor access (only 'viable' candidates receive
 *   institutional funding). These mechanisms are mutually reinforcing: a
 *   candidate below the threshold cannot access media to raise their polling
 *   numbers, cannot qualify for debates to raise their visibility, and cannot
 *   attract donors without demonstrated viability. The constraint's
 *   extractiveness has increased over the past 10 years as media
 *   concentration (fewer outlets, greater reliance on polling aggregation)
 *   has amplified the viability narrative, and as debate commission
 *   thresholds have risen. The theater ratio is high because much of the
 *   enforcement is performative: the 'serious candidate' criterion is
 *   articulated, but actual candidate quality does not correlate with
 *   viability thresholds (many major-party nominees fail the actual
 *   governance test). Electoral reform movements (ranked-choice voting, open
 *   primaries, ballot-access reform) are creating genuine alternative
 *   pathways that represent a true sunset clause for the current threshold
 *   structure.
 *
 * KEY AGENTS:
 *   - Third-Party Candidates: Primary victims (powerless/trapped) — structurally barred from all mechanisms of viability; experience catch-22 suppression
 *   - Swing Voters: Secondary victims (moderate/constrained) — face reduced choice set and information asymmetry about alternatives; can exit through abstention or strategic voting at cost
 *   - Major Party Duopoly: Primary beneficiaries (institutional/arbitrage) — benefit from fragmentation prevention and predictable coalition-building; capture policy space between parties
 *   - Institutional Gatekeepers: Secondary beneficiaries (institutional/constrained) — ballot commissions, debate commissions, FEC; maintain thresholds through regulatory inertia
 *   - Electoral Apparatus: Tertiary actor (institutional/constrained) — enforces thresholds through ballot-access rules, debate qualification, and coverage norms; perceives own rules as arbitrary
 *   - Electoral Reform Movement: Organized challenger (organized/constrained) — actively building alternative pathways (ranked-choice, open primary, fusion voting) with structural sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional thresholds as inherent electoral mathematics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electoral_viability_threshold, 0.58).
domain_priors:suppression_score(electoral_viability_threshold, 0.65).
domain_priors:theater_ratio(electoral_viability_threshold, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electoral_viability_threshold, extractiveness, 0.58).
narrative_ontology:constraint_metric(electoral_viability_threshold, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(electoral_viability_threshold, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electoral_viability_threshold, tangled_rope).
narrative_ontology:human_readable(electoral_viability_threshold, "Electoral Viability Threshold").
narrative_ontology:topic_domain(electoral_viability_threshold, "political_economy/electoral_systems").

domain_priors:requires_active_enforcement(electoral_viability_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electoral_viability_threshold, major_party_duopoly).
narrative_ontology:constraint_beneficiary(electoral_viability_threshold, institutional_gatekeepers).
narrative_ontology:constraint_victim(electoral_viability_threshold, third_party_candidates).
narrative_ontology:constraint_victim(electoral_viability_threshold, voter_choice_diversity).
narrative_ontology:constraint_victim(electoral_viability_threshold, policy_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THIRD-PARTY CANDIDATE (SNARE) — Structurally barred from ballot access, media coverage, debate stages, and funding mechanisms. Cannot exit the constraint without abandoning candidacy entirely. Experiences extraction as total exclusion: must win 5% nationally (ballot access) or 15% (debate threshold) despite zero media coverage and donor inaccessibility — a catch-22 mechanism. Maximum suppression with zero degrees of freedom.
constraint_indexing:constraint_classification(electoral_viability_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SWING VOTER (TANGLED ROPE) — Faces genuine coordination problem (two-party system reduces transaction costs for electoral participation and signal clarity) but also faces extraction: viability threshold suppresses visibility of policy alternatives, constraining voter choice set. Can exit through strategic voting or abstention, but at cost of reduced preferred-outcome probability. Moderate extraction with some agency.
constraint_indexing:constraint_classification(electoral_viability_threshold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAJOR PARTY DUOPOLY (ROPE) — Benefits from viability threshold as a coordination mechanism: two-party system reduces fragmentation, stabilizes coalition-building, and guarantees turnout incentives. Experiences the constraint as solving a genuine collective action problem. Net beneficiary with full exit option (could support proportional representation but choose not to) and arbitrage access (can shift policy toward center to capture swing voters).
constraint_indexing:constraint_classification(electoral_viability_threshold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ELECTORAL APPARATUS (PITON) — The institutional machinery (ballot access rules, debate commissions, polling thresholds, FEC regulations) enforces the viability threshold through legislative inertia rather than active coordination benefit. Theater ratio is high: regulations ostensibly ensure 'serious candidates' (performative criterion), but actually function to exclude dynamic challengers. The apparatus perceives its own rules as arbitrary and sometimes proposes reform, yet enforcement persists through institutional path-dependency and major-party pressure.
constraint_indexing:constraint_classification(electoral_viability_threshold, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ELECTORAL REFORM MOVEMENT (SCAFFOLD) — Organized actors (ranked-choice voting campaigns, open primary advocates, ballot-access reformers) see the viability threshold as a temporary institutional structure with a genuine sunset clause. Ranked-choice voting, fusion voting, and open-primary reforms are actively narrowing the threshold by changing the mechanism itself. Low effective extraction because this coalition has agency, resources, and a structural exit pathway. Suppression is declining as reforms spread.
constraint_indexing:constraint_classification(electoral_viability_threshold, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some electoral viability threshold is inherent to any voting system: candidates below a certain vote-getting capacity cannot plausibly form government or influence policy. This perspective treats the threshold as an immutable mathematical property of electoral systems. However, this naturalizes what is actually a contingent institutional parameter (5% for ballot access, 15% for debates, 270 for electoral college) — different electoral systems produce different thresholds. The engine's false summit detector identifies this as naturalization of design choices.
constraint_indexing:constraint_classification(electoral_viability_threshold, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electoral_viability_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(electoral_viability_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(electoral_viability_threshold, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(electoral_viability_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(electoral_viability_threshold, TR),
    TR >= 0.70.

:- end_tests(electoral_viability_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The viability threshold generates asymmetric extraction concentrated on third-party candidates and voters seeking policy alternatives. The original research showed 0.45 at t=0 (when the constraint was primarily structural rule-based) rising to 0.58 at t=10 (as media amplification and debate commission thresholds have tightened the mechanism). This increase reflects not a change in the formal rules but an intensification of their enforcement through narrative and coordination mechanisms. Suppression (0.65): High. Barriers to third-party viability include ballot-access requirements (signatures, filing fees), debate-qualification thresholds (15% polling), media access (viability-driven coverage), donor access (institutional funding flows to 'viable' candidates only), and strategic voting disincentives (voter perception that third-party vote 'wastes' their ballot). Importantly, suppression is not total — some third-party candidates achieve ballot access and limited visibility, and some voters do support third parties despite viability penalties. Theater ratio (0.68): High. The enforcement of viability is substantially performative: 'serious candidate' criteria (demonstrated capacity, organization, message clarity) are articulated, but actual correlation with governance success is weak. Debate commission thresholds ostensibly identify serious candidates but actually exclude candidates with real policy followings. Media viability framing creates narrative performance independent of actual vote-getting capacity — a candidate can be 'not viable' in media frames despite having real support.
 *
 * PERSPECTIVAL GAP:
 *   The third-party candidate perspective (Snare) directly contradicts the major-party duopoly perspective (Rope). Both perspectives operate from institutional power atoms with different exit options (trapped vs arbitrage), producing opposite directional relationships to the same constraint. The duopoly benefits from the threshold as a coordination mechanism; the third party bears it as pure extraction. This gap is not resolvable into a single 'true' type — it reveals the hybrid nature of the constraint. The institutional electoral apparatus perceives the constraint as degraded (Piton) — maintaining performance without function — while the reform movement sees it as temporary (Scaffold) with real institutional sunset. The swing voter perspective bridges duopoly and third-party: they experience genuine coordination benefits (clarity, coalition stability) alongside real extraction (suppressed choice). This mixed experience places them in Tangled Rope territory. The analytical observer's Mountain classification fails the false summit test: civilizational-scale electoral mathematics does not require a 15% polling threshold; it requires only some threshold, which varies by system design.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation for electoral viability constraint: Third-party candidates are victims in a trapped position (d→0.95), experiencing full f(d)≈1.42 extraction amplification. Swing voters are victims in constrained position (d→0.70), experiencing f(d)≈1.00 amplification. Major-party duopoly are beneficiaries with arbitrage exit (d→0.10), experiencing f(d)≈-0.01 (institutional subsidy). Debate commissions and ballot authorities are institutional actors with constrained exit who enforce but do not benefit (d→0.55, f(d)≈0.65). Electoral reform organizations are organized challengers with constrained exit and moderate targeting (d→0.60, f(d)≈0.75). The scope modifier σ(S) is 1.0 at national scale (the natural enforcement level for electoral thresholds), so χ = ε × f(d) × 1.0 for all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: Electoral viability threshold resolves mandatrophy by showing that all six types are legitimate from different structural positions. The mandatrophy is not 'which type is correct?' but 'from which position are you measuring?' This resolves the ostensible contradiction between 'the threshold is essential coordination' (duopoly view, Rope) and 'the threshold is pure extraction' (third-party view, Snare). Both are true — they describe different extraction flows through the same constraint. The threshold coordinates major-party coalitions while extracting from third-party competitors and suppressing voter preferences. The high theater ratio (0.68) indicates that enforcement relies significantly on narrative and institutional inertia rather than necessity — 'serious candidate' standards are applied inconsistently, debate thresholds vary by commissions, and media viability framing is contingent on coverage patterns. The Piton classification of the electoral apparatus confirms this: the rules persist through institutional path-dependency, not because they solve the coordination problem better than alternatives. The Scaffold perspective from the reform movement indicates that the constraint is structurally time-limited: ranked-choice voting (Maine, Alaska, Colorado), fusion voting (New York), and open-primary reforms (California) are building genuine alternative pathways. These are not marginal tweaks but structural changes to how viability is determined. The analytical observer's false summit reveals the naturalizing move: framing 'some threshold must exist' as 'this specific 15% threshold is immutable.' It is not. The threshold is a parameter choice, contingent on electoral system design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_coordination_vs_extraction,
    'Is the viability threshold a coordination mechanism preventing fragmentation, or primarily an extraction mechanism maintaining duopoly rent-seeking?',
    'Cross-national analysis: compare electoral concentration and policy diversity in two-party systems with viability thresholds vs proportional-representation systems with lower thresholds; track outcomes across system reforms (e.g., ranked-choice adoption, open-primary shifts)',
    'If primarily coordination: major-party constraint and voter suppression are byproducts of legitimate stability needs; threshold can be marginally optimized but not eliminated. If primarily extraction: threshold is maintaining duopoly power; reform to proportional or ranked-choice voting would reveal suppressed policy demand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_coordination_vs_extraction, empirical, 'Whether threshold functions as coordination or extraction mechanism').

omega_variable(
    media_coverage_amplification,
    'Does media coverage of viability (polling-driven narrative) amplify the threshold effect, or merely reflect underlying structural barriers?',
    'Experimental analysis: comparison of third-party performance in media markets with vs without viability-threshold framing; longitudinal tracking of media coverage thresholds vs actual vote-getting capacity; analysis of feedback loops between polling, coverage, and donor access',
    'If media-amplified: reducing coverage of viability narrative could materially lower effective threshold; third-party candidates are suppressed by narrative rather than solely by structural rules. If structural: media reflects real barriers and cannot change outcome through coverage alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(media_coverage_amplification, empirical, 'Whether media viability framing amplifies or reflects threshold effects').

omega_variable(
    swing_voter_preference_aggregation,
    'To what extent do swing voters prefer the two-party constraint as a simplifying heuristic vs being suppressed by information asymmetry about third-party alternatives?',
    'Survey research: assess voter preference for two-party vs multi-party systems under full information about alternatives; behavioral experiments on choice architecture effects; longitudinal tracking of voter preference shifts during electoral-reform campaigns',
    'If preference-based: viability threshold reflects genuine voter coordination demand; lowering it without addressing preference-shifting may not increase third-party support. If suppression-based: voters have latent demand for alternatives that threshold currently masks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(swing_voter_preference_aggregation, empirical, 'Whether swing voters prefer two-party constraint or are suppressed by it').

omega_variable(
    coalition_viability_of_third_parties,
    'Can third parties achieve legislative viability (coalition-building capacity) below the current electoral viability threshold, or does threshold reflect genuine coalition-formation mathematics?',
    'Institutional analysis: examine coalition-building mechanics in multi-party legislatures; compare governing-coalition formation rates in proportional-representation systems vs two-party systems; simulate coalition outcomes under different viability thresholds',
    'If threshold is mathematically necessary: lowering it creates gridlock or unstable governments; extraction may be justified cost of stability. If threshold is political choice: many legislative systems function at lower thresholds; extraction is not inherent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coalition_viability_of_third_parties, empirical, 'Whether electoral viability threshold reflects coalition-formation mathematics').

omega_variable(
    reform_sunset_trajectory,
    'Are electoral reform movements creating a genuine structural transition (sunset for current viability regime), or do they generate temporary perturbations that reset to equilibrium?',
    'Historical analysis of ballot-access, debate, and primary reforms; tracking of policy outcomes post-reform; longitudinal measurement of third-party vote share and institutional presence in reformed vs unreformed jurisdictions',
    'If genuine sunset: scaffold classification is correct; viability threshold is temporary institutional form. If oscillatory: reforms generate temporary expansions followed by duopoly consolidation; threshold is structurally stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_sunset_trajectory, empirical, 'Whether reform movements create structural transition or oscillatory equilibrium').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electoral_viability_threshold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(evt_tr_t0, electoral_viability_threshold, theater_ratio, 0, 0.55).
narrative_ontology:measurement(evt_tr_t5, electoral_viability_threshold, theater_ratio, 5, 0.62).
narrative_ontology:measurement(evt_tr_t10, electoral_viability_threshold, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(evt_be_t0, electoral_viability_threshold, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(evt_be_t5, electoral_viability_threshold, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(evt_be_t10, electoral_viability_threshold, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electoral_viability_threshold, resource_allocation).
narrative_ontology:affects_constraint(electoral_viability_threshold, two_party_system_lock).
narrative_ontology:affects_constraint(electoral_viability_threshold, campaign_finance_duopoly).
narrative_ontology:affects_constraint(electoral_viability_threshold, media_narrative_gatekeeping).

% DUAL FORMULATION NOTE:
% The electoral viability threshold is downstream of formal ballot-access and debate-commission rules but represents a distinct structural constraint with its own enforcement mechanisms (narrative, media coordination, donor access). The upstream constraints (two-party system, campaign finance rules, media ownership) have their own extractiveness values; the viability threshold has its own (0.58) reflecting the specific coordination-extraction hybrid it instantiates. All three constraints are linked: viability threshold depends on and reinforces campaign finance duopoly and media gatekeeping.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(electoral_viability_threshold, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
