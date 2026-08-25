% ============================================================================
% CONSTRAINT STORY: beatability_of_the_take_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beatability_of_the_take_flat_control, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: beatability_of_the_take_flat_control
 *   human_readable: Parimutuel Takeout Structure (~20% Pool Extraction)
 *   domain: quantitative_finance/gambling_theory/market_microstructure
 *
 * SUMMARY:
 *   The parimutuel take is a structural fact everyone agrees on: roughly 20%
 *   of every pool is removed before payout, redistributed to purses, track
 *   operations, and state revenue. This story treats the take as a single
 *   flat constraint rather than decomposing it into the four interpretive
 *   readings the source material identifies (mug's game / flow-mechanics
 *   non-event / unknowing liability / beatable meta-prediction problem).
 *   Those four readings are not separate constraints with different epsilon —
 *   they are perspectival stances on ONE fixed extraction rate, and the
 *   disagreement is authored here as seat-level divergence (professional
 *   syndicates vs. recreational bettors vs. operators) rather than as sibling
 *   constraint stories. The take itself does not change based on which stance
 *   a bettor holds; what changes is whether a given bettor, given their
 *   capital, information, and discipline, can extract a net positive expected
 *   value from the remaining 80% of the pool.
 *
 * KEY AGENTS:
 *   - track_and_tote_operators: agenda_setter/beneficiary (institutional/arbitrage) — collects the take regardless of outcome
 *   - state_racing_regulators: beneficiary/agenda_setter (institutional/analytical) — sets the rate by statute, collects a revenue share
 *   - professional_syndicate_bettors: beneficiary (organized/mobile) — treats the take as a known cost in a beatable meta-prediction game
 *   - recreational_bettors: payer (powerless/constrained) — bears the extraction, often without modeling its compounding effect
 *   - casual_handicappers: payer/beneficiary (moderate/constrained) — believes intermittently that the take is surmountable
 *   - academic_market_microstructure_researchers: observer (analytical/analytical) — documents that beatability is real but capital- and skill-gated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beatability_of_the_take_flat_control, 0.62).
domain_priors:suppression_score(beatability_of_the_take_flat_control, 0.35).
domain_priors:theater_ratio(beatability_of_the_take_flat_control, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beatability_of_the_take_flat_control, extractiveness, 0.62).
narrative_ontology:constraint_metric(beatability_of_the_take_flat_control, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(beatability_of_the_take_flat_control, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beatability_of_the_take_flat_control, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(beatability_of_the_take_flat_control, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beatability_of_the_take_flat_control, tangled_rope).
narrative_ontology:human_readable(beatability_of_the_take_flat_control, "Parimutuel Takeout Structure (~20% Pool Extraction)").
narrative_ontology:topic_domain(beatability_of_the_take_flat_control, "quantitative_finance/gambling_theory/market_microstructure").

domain_priors:requires_active_enforcement(beatability_of_the_take_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(beatability_of_the_take_flat_control, beatability_of_the_take).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beatability_of_the_take_flat_control, track_and_tote_operators).
narrative_ontology:constraint_beneficiary(beatability_of_the_take_flat_control, state_racing_regulators).
narrative_ontology:constraint_beneficiary(beatability_of_the_take_flat_control, professional_syndicate_bettors).
narrative_ontology:constraint_victim(beatability_of_the_take_flat_control, recreational_bettors).
narrative_ontology:constraint_victim(beatability_of_the_take_flat_control, casual_handicappers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(beatability_of_the_take_flat_control, casual_handicappers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and collects the fixed takeout percentage from every pool before any payout is calculated. Justifies the take as funding purse money, track operations, and regulatory licensing fees. Has no exposure to which horse wins — revenue is guaranteed regardless of outcome, making the operator structurally indifferent to the beatability question that consumes every other seat.
narrative_ontology:constraint_stakeholder(beatability_of_the_take_flat_control, track_and_tote_operators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(beatability_of_the_take_flat_control, track_and_tote_operators, beneficiary).

% Authorizes the takeout rate by statute, collects a share for state revenue and industry subsidy, and treats the rate as a settled policy fact rather than an open empirical question. Rarely revisits the rate downward even when handle declines, because the revenue share is baked into budget assumptions.
narrative_ontology:constraint_stakeholder(beatability_of_the_take_flat_control, state_racing_regulators, beneficiary,
    institutional, generational, analytical, regional).
narrative_ontology:stakeholder_secondary_role(beatability_of_the_take_flat_control, state_racing_regulators, agenda_setter).

% Pool capital, build predictive models, and bet late into pools to exploit pricing inefficiencies created by less-informed money. They treat the fixed take as a known cost of a beatable meta-prediction game — the pool's public odds are a noisy consensus estimate they can out-forecast net of the ~20% vig, given sufficient edge and volume. Can walk away from any single track or jurisdiction without loss of livelihood.
narrative_ontology:constraint_stakeholder(beatability_of_the_take_flat_control, professional_syndicate_bettors, beneficiary,
    organized, biographical, mobile, national).

% Bet small amounts for entertainment, often without modeling the takeout's compounding effect across a betting card. Many experience the constraint as a mug's game they nonetheless keep playing; others experience it as a background flow-mechanics fact they don't register as a personal cost at all — an unknowing liability accrued bet by bet. Exit is nominally free (no one forces attendance) but socially and psychologically constrained by habit, sunk-cost framing, and entertainment value that masks the extraction rate.
narrative_ontology:constraint_stakeholder(beatability_of_the_take_flat_control, recreational_bettors, payer,
    powerless, immediate, constrained, local).

% Study form and odds semi-seriously, believe they can beat specific races or exotic bets, and experience occasional wins that reinforce the belief the take is surmountable. In aggregate they lose to the take over any sustained sample, but individual sessions can mask this, keeping them in the game longer than a purely rational actor would stay.
narrative_ontology:constraint_stakeholder(beatability_of_the_take_flat_control, casual_handicappers, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(beatability_of_the_take_flat_control, casual_handicappers, beneficiary).

% Study parimutuel markets as a clean laboratory for market efficiency and favorite-longshot bias research. Their published findings document that the pool is beatable at the margin for sufficiently skilled, well-capitalized bettors net of takeout, but unbeatable in expectation for the median participant — a finding contested in its policy implications but not in its arithmetic.
narrative_ontology:constraint_stakeholder(beatability_of_the_take_flat_control, academic_market_microstructure_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beatability_of_the_take_flat_control, track_and_tote_operators).
narrative_ontology:fixing_cost_class(beatability_of_the_take_flat_control, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The takeout funds purse money (which sustains the racing product itself), track operating costs, and state regulatory oversight — without it, no pool exists for anyone to bet into, and the sport's economic base collapses. This is a genuine coordination problem: someone must fund the infrastructure that makes any pool possible.
% TRANSFER_FUNCTION: Moves roughly 20% of every dollar wagered out of the payout pool before any bettor receives anything, redirecting it to track operators, purse accounts, and state revenue, funded overwhelmingly by the aggregate losses of bettors who wager more than they recoup — disproportionately recreational and casual bettors rather than professional syndicates who extract net positive returns from the remaining pool.
% ABSENT_VOICES: Bettors who have already left the game — those who did the arithmetic, recognized the takeout's compounding effect, and stopped — are not present to argue the pool is structurally unbeatable for anyone without syndicate-level capital and modeling; their absence leaves the debate dominated by those still playing, who are systematically the ones who believe (rightly or wrongly) that they personally can beat it.
% DISAPPEARANCE_RATIONALE: If the takeout vanished overnight, purse money would collapse, tracks would close within a season without an alternative funding mechanism, and professional syndicates would lose the pool structure their edge depends on — but the underlying activity (betting on race outcomes) would likely reorganize into fixed-odds or exchange-based wagering with a different, possibly lower, extraction structure. The take itself is a policy choice, not a fact of the world.
% FOUNDING_PROBLEM: Racetracks and states needed a self-funding mechanism to pay purses, cover track operations, and collect wagering tax revenue without relying on general taxation or admission fees alone, while also creating a pooled-odds mechanism where payouts didn't require a bookmaker to set prices or bear risk.
% FOUNDING_PROBLEM_CORROBORATION: Independent industry economists and state fiscal offices attest that purse funding and track solvency genuinely depend on takeout revenue in most jurisdictions — this is corroborated outside the operators and regulators who collect it, via published state gaming-revenue audits and horse-industry economic impact studies. However, the RATE itself (why ~20% rather than a lower figure) is corroborated by no one outside the beneficiary set; no independent audit has established that current rates are the minimum necessary to fund the stated purposes.
narrative_ontology:disappearance_verdict(beatability_of_the_take_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(beatability_of_the_take_flat_control, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beatability_of_the_take_flat_control, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-25',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(beatability_of_the_take_flat_control, 'none', 1).
narrative_ontology:epsilon_provenance(beatability_of_the_take_flat_control, 0.62, 'claude-sonnet-5', 'benter_hkjc_parimutuel_2026_20260825_125025', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beatability_of_the_take_flat_control_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(beatability_of_the_take_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(beatability_of_the_take_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62 — meaningfully above the bare 20% headline rate, because the effective extraction compounds across a betting card and because the fixed take is levied on gross handle regardless of individual outcome, meaning a bettor who breaks even on race selection still loses the full take on every dollar recycled into subsequent bets. Suppression is comparatively low (0.35) because no one is coerced into betting and exit is nominally free — the suppression that exists is mostly informational (the take's compounding effect is not salient at the point of sale) rather than coercive. Theater ratio (0.4) reflects that a meaningful share of the take's public justification — 'supporting the sport,' 'funding purses' — is genuine in aggregate but increasingly decoupled from what any individual jurisdiction's rate-setting process actually optimizes for, which is closer to maximizing extractable revenue within the range bettors will tolerate before exiting to other gambling products.
 *
 * PERSPECTIVAL GAP:
 *   The professional syndicate seat and the recreational bettor seat look at the identical 20% take and reach opposite structural verdicts — not because the take differs, but because their capital, modeling capacity, and exit options differ. A syndicate bettor with predictive edge treats the take as a beatable cost of doing business (net positive EV survives the vig); a recreational bettor betting on hunches treats the same take as either an irrelevant flow-mechanics fact (they don't compute it) or a mug's-game certainty (they've done the arithmetic and concluded it's unbeatable for them specifically). Both readings are locally correct for their respective seats — the engine's per-seat computation should reflect that the SAME fixed ε produces divergent effective outcomes depending on directionality, which here tracks informational and capital asymmetry rather than institutional power alone.
 *
 * DIRECTIONALITY LOGIC:
 *   Track operators and regulators are structural full beneficiaries: they collect the take regardless of who wins, making their directionality maximally low (d near 0) — they have no exposure to the beatability question at all. Professional syndicates sit close to symmetric-to-beneficiary: they pay the take like everyone else but their modeling edge converts a nominally extractive structure into a net-positive game for them specifically, which is why they are listed as beneficiaries rather than payers despite technically wagering into the same pool. Recreational bettors and casual handicappers are the structural targets: the take is levied on their wagers with no offsetting edge, and their exit is constrained by entertainment value, habit, and the illusion of beatability reinforced by occasional wins.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — funding purses and track operations without general taxation — remains genuinely live; the sport would not survive its removal in the current funding architecture. This prevents a simple snare classification: there IS a real coordination function underneath the extraction, which is why tangled_rope (not snare) is the authored claim. But the RATE has outlived scrutiny in a way the FUNCTION has not: no independent party outside the beneficiary set attests that ~20% is the minimum necessary rate rather than the maximum tolerable one. That is the specific mandatrophy risk — not that the take should be zero, but that the rate-setting process no longer connects the take's magnitude to its stated justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beatability_is_capital_gated_not_universal,
    'Is the parimutuel pool genuinely beatable net of takeout for a bettor with sufficient skill, or does the ~20% take make it a negative-expectation game for everyone regardless of skill, with syndicate ''success'' actually reflecting non-wagering revenue (data sales, rebates, or subsidies) rather than pool-beating edge?',
    'Audited long-run ROI data from professional syndicates, net of any rebate or subsidy arrangements with tracks, compared against a null model of random betting minus takeout. Distinguish pool-beating skill from rebate-driven profitability, which is a different economic mechanism entirely.',
    'If syndicate profitability is substantially rebate-driven rather than skill-driven, the ''beatable meta-prediction problem'' reading is a cover story for a separate, non-transparent extraction-sharing arrangement between operators and large bettors, which would push the classification toward snare for recreational bettors specifically (extraction subsidizes a favored counterparty rather than funding genuine coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beatability_is_capital_gated_not_universal, empirical, 'Whether professional beatability reflects genuine skill or an undisclosed rebate structure.').

omega_variable(
    rate_setting_process_independence,
    'Is the ~20% takeout rate set by an empirical process tied to the actual minimum cost of funding purses, track operations, and regulatory oversight, or is it set by political/institutional inertia and revenue maximization with only loose reference to actual funding needs?',
    'Comparative analysis of takeout rates across jurisdictions with different regulatory processes; correlation (or lack thereof) between rate changes and documented funding shortfalls versus revenue-maximization studies commissioned by operators.',
    'If rate-setting is decoupled from funding need, the coordination-function justification is real but oversized relative to what it actually requires, meaning a portion of the 0.62 extractiveness score is unjustified rent rather than coordination cost — sharpening the tangled_rope classification toward the extraction pole.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rate_setting_process_independence, conceptual, 'Whether the takeout rate tracks funding necessity or institutional revenue maximization.').

omega_variable(
    recreational_bettor_awareness_state,
    'Do recreational bettors experience the take as an unknowing liability (they have not computed its compounding effect) or as a known, accepted cost of entertainment (they know and don''t care because the value is the experience, not the expected return)?',
    'Survey data or behavioral studies distinguishing bettors who can state the approximate takeout rate and its implications from those who cannot, cross-referenced with self-reported motivations for betting.',
    'If predominantly unknowing, the suppression metric understates the informational asymmetry component and the constraint functions closer to a snare for this population; if predominantly knowing-and-accepting, the entertainment-coordination framing is more defensible and closer to a genuine (if lopsided) rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recreational_bettor_awareness_state, empirical, 'Whether recreational bettor exposure to the take is informed acceptance or unknowing liability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beatability_of_the_take_flat_control, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beat_tr_t0, beatability_of_the_take_flat_control, theater_ratio, 0, 0.28).
narrative_ontology:measurement(beat_tr_t8, beatability_of_the_take_flat_control, theater_ratio, 8, 0.31).
narrative_ontology:measurement(beat_tr_t16, beatability_of_the_take_flat_control, theater_ratio, 16, 0.34).
narrative_ontology:measurement(beat_tr_t24, beatability_of_the_take_flat_control, theater_ratio, 24, 0.36).
narrative_ontology:measurement(beat_tr_t32, beatability_of_the_take_flat_control, theater_ratio, 32, 0.38).
narrative_ontology:measurement(beat_tr_t40, beatability_of_the_take_flat_control, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(beat_be_t0, beatability_of_the_take_flat_control, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(beat_be_t8, beatability_of_the_take_flat_control, base_extractiveness, 8, 0.57).
narrative_ontology:measurement(beat_be_t16, beatability_of_the_take_flat_control, base_extractiveness, 16, 0.59).
narrative_ontology:measurement(beat_be_t24, beatability_of_the_take_flat_control, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(beat_be_t32, beatability_of_the_take_flat_control, base_extractiveness, 32, 0.61).
narrative_ontology:measurement(beat_be_t40, beatability_of_the_take_flat_control, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(beat_su_t0, beatability_of_the_take_flat_control, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(beat_su_t8, beatability_of_the_take_flat_control, suppression_requirement, 8, 0.31).
narrative_ontology:measurement(beat_su_t16, beatability_of_the_take_flat_control, suppression_requirement, 16, 0.32).
narrative_ontology:measurement(beat_su_t24, beatability_of_the_take_flat_control, suppression_requirement, 24, 0.33).
narrative_ontology:measurement(beat_su_t32, beatability_of_the_take_flat_control, suppression_requirement, 32, 0.34).
narrative_ontology:measurement(beat_su_t40, beatability_of_the_take_flat_control, suppression_requirement, 40, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beatability_of_the_take_flat_control, resource_allocation).
narrative_ontology:boltzmann_floor_override(beatability_of_the_take_flat_control, 0.12).

% DUAL FORMULATION NOTE:
% This is authored as the FLAT (undecomposed) construction of the beatability-of-the-take substrate. The four interpretive stances named in the source material (mug's game, flow-mechanics non-event, unknowing liability, beatable meta-prediction problem) are NOT authored as sibling reading constraints here — per the construction-perturbation instruction, they are folded into seat-level perspectival divergence (professional_syndicate_bettors vs. recreational_bettors vs. track_and_tote_operators) and into omega variables documenting the open empirical questions each stance implicitly answers differently. A decomposed version of this substrate would instead produce up to four separate constraint_id files, each with its own claimed_type and stakeholder set, linked via affects_constraints — that decomposition is deliberately NOT performed in this flat-control story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
