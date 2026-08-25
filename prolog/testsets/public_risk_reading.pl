% ============================================================================
% CONSTRAINT STORY: public_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_risk_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: public_risk_reading
 *   human_readable: Public Odds as Undetected Risk-Holding (Favourite-Longshot Bias Reading)
 *   domain: quantitative_finance/gambling_theory/market_microstructure
 *
 * SUMMARY:
 *   This constraint isolates one reading of the 'is the take beatable'
 *   question: the public's aggregate betting behavior encodes a systematic,
 *   well-documented probability error (favourite-longshot bias — longshots
 *   overpriced relative to true frequency, favourites underpriced), and the
 *   public cannot detect this error because it only exists at the population
 *   level, not in any individual bet. Anyone able to measure the divergence
 *   between the public's implied probabilities and true frequencies can
 *   extract from it, net of the take. This is distinct from the
 *   flow-extraction reading (which focuses on the mechanical take itself),
 *   the folk-mountain reading (which treats 'the house always wins' as brute
 *   natural law), and the meta-prediction reading (which asks whether beating
 *   the take requires beating other sharps, not the public). Here the victim
 *   is specifically the public AS A CLASS carrying model error, and the
 *   beneficiary is specifically whoever can measure that error — the take is
 *   present but secondary to this reading's central threat.
 *
 * KEY AGENTS:
 *   - public_bettors_as_a_class: primary risk-holder (powerless/trapped) — carries undetectable model error
 *   - sharp_bettors_with_calibrated_models: primary beneficiary (moderate/arbitrage) — measures and exploits the divergence
 *   - syndicate_arbitrageurs: scaled beneficiary (organized/arbitrage) — converts the bias into stable extraction at volume
 *   - bookmakers_and_exchanges: agenda-setter and secondary beneficiary (institutional/arbitrage) — sets odds, collects take, has no incentive to correct the bias
 *   - quantitative_analysts: analytical observer — documents the bias empirically from outside the betting economy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_risk_reading, 0.71).
domain_priors:suppression_score(public_risk_reading, 0.42).
domain_priors:theater_ratio(public_risk_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_risk_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(public_risk_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(public_risk_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_risk_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(public_risk_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_risk_reading, snare).
narrative_ontology:human_readable(public_risk_reading, "Public Odds as Undetected Risk-Holding (Favourite-Longshot Bias Reading)").
narrative_ontology:topic_domain(public_risk_reading, "quantitative_finance/gambling_theory/market_microstructure").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_risk_reading, '380dfd22-a1ca-4db5-8258-ec41eb1900c3').
narrative_ontology:cs_kernel_codification('380dfd22-a1ca-4db5-8258-ec41eb1900c3', distributed).
narrative_ontology:cs_authority_grounding('380dfd22-a1ca-4db5-8258-ec41eb1900c3', diffuse_epistemic).
narrative_ontology:cs_reading_relation('380dfd22-a1ca-4db5-8258-ec41eb1900c3', public_risk_reading__flow_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('380dfd22-a1ca-4db5-8258-ec41eb1900c3', public_risk_reading__folk_mountain_reading, influences).
narrative_ontology:cs_reading_relation('380dfd22-a1ca-4db5-8258-ec41eb1900c3', public_risk_reading__meta_prediction_reading, coexists_with).
narrative_ontology:cs_axiom('380dfd22-a1ca-4db5-8258-ec41eb1900c3', foundational, public_probability_estimates_are_systematically_biased).
narrative_ontology:cs_axiom_status(public_probability_estimates_are_systematically_biased, holdable).
narrative_ontology:cs_axiom_grounding('380dfd22-a1ca-4db5-8258-ec41eb1900c3', public_probability_estimates_are_systematically_biased, empirically_contingent).
narrative_ontology:cs_axiom('380dfd22-a1ca-4db5-8258-ec41eb1900c3', secondary, bias_is_undetectable_at_individual_bet_level).
narrative_ontology:cs_axiom_status(bias_is_undetectable_at_individual_bet_level, holdable).
narrative_ontology:cs_axiom_grounding('380dfd22-a1ca-4db5-8258-ec41eb1900c3', bias_is_undetectable_at_individual_bet_level, empirically_contingent).
narrative_ontology:cs_created_at('380dfd22-a1ca-4db5-8258-ec41eb1900c3', '').
narrative_ontology:cs_kernel_id(public_risk_reading, beatability_of_the_take).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_risk_reading, sharp_bettors_with_calibrated_models).
narrative_ontology:constraint_beneficiary(public_risk_reading, syndicate_arbitrageurs).
narrative_ontology:constraint_victim(public_risk_reading, public_bettors_as_a_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(public_risk_reading, bookmakers_and_exchanges).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Places bets using intuitive probability judgments that systematically overweight longshots and underweight favourites. Cannot observe its own aggregate bias because the bias only shows up as a statistical regularity across thousands of bets, not in any single wager. Believes it is taking a fair-ish gamble against the house; is actually carrying a model-error tax on top of the take, without the tools to detect either.
narrative_ontology:constraint_stakeholder(public_risk_reading, public_bettors_as_a_class, payer,
    powerless, immediate, trapped, national).

% Builds or buys probability models that are closer to true frequencies than the crowd's intuitive odds. Bets selectively into the mispricing the crowd's bias creates — backing relative favourites the crowd underprices, laying relative longshots the crowd overprices. Can walk away from any single market where the divergence is too small to clear the take; exit is cheap because the edge is portable across venues and events.
narrative_ontology:constraint_stakeholder(public_risk_reading, sharp_bettors_with_calibrated_models, beneficiary,
    moderate, biographical, arbitrage, national).

% Pools capital and modeling talent to detect the favourite-longshot bias at scale across many markets simultaneously, converting the public's aggregate error into a stable long-run edge net of the take. Faces limited resistance from operators as long as volume stays under thresholds that trigger account restriction; can redeploy capital to whichever market currently shows the largest crowd-model divergence.
narrative_ontology:constraint_stakeholder(public_risk_reading, syndicate_arbitrageurs, beneficiary,
    organized, biographical, arbitrage, continental).

% Sets the odds (or facilitates the market that sets them), collects the take regardless of who wins, and quietly benefits from the public's bias insofar as it stabilizes the book and dampens the effective edge sharps can extract without adjusting lines. Does not need the bias to exist to profit, but does not correct it either, since correcting it would shrink the crowd's willingness to bet on emotionally salient longshots.
narrative_ontology:constraint_stakeholder(public_risk_reading, bookmakers_and_exchanges, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(public_risk_reading, bookmakers_and_exchanges, beneficiary).

% Oversees licensing and disclosure of the take (the visible commission) but has no mandate or metric for the invisible bias layered underneath it, since favourite-longshot bias is a property of aggregate market behavior rather than any single disclosed term. Their absence from the bias conversation is structural, not an oversight of will.
narrative_ontology:constraint_stakeholder(public_risk_reading, regulators_and_consumer_protection_bodies, excluded,
    institutional, generational, analytical, national).

% Documents the favourite-longshot bias empirically across decades of racing, sports, and prediction markets. Publishes the finding but has no direct stake in either extracting from it or protecting the public from it; supplies the evidentiary basis on which the sharp/syndicate edge and the public's exposure are both established.
narrative_ontology:constraint_stakeholder(public_risk_reading, quantitative_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_risk_reading, syndicate_arbitrageurs).
narrative_ontology:fixing_cost_class(public_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregating dispersed public judgment into a single tradeable probability (the odds line) does solve a genuine coordination problem — it lets a market clear on an event's outcome without a central authority dictating the 'true' probability.
% TRANSFER_FUNCTION: Moves expected value from the pool of public bettors, whose aggregate probability estimates are systematically biased (overpricing longshots, underpricing favourites), to whichever agents can measure that bias and bet against it — sharp individuals and syndicates — net of the take collected by the house.
% ABSENT_VOICES: The public bettor as an individual never sees their own bias; it is a population-level statistical fact invisible at the level of any single wager. Consumer-protection regulators, whose remit covers the disclosed take, have no comparable disclosure regime for the undisclosed, empirically-derived bias — they are structurally absent from a conversation that requires aggregate statistical literacy they are not chartered to hold.
% DISAPPEARANCE_RATIONALE: If the public's probability estimates were suddenly unbiased (calibrated), the arbitrage edge sharps and syndicates currently extract from favourite-longshot mispricing would collapse to whatever remains after the take alone; syndicate capital would migrate to markets that still show the divergence, and the public would face a fair-ish (still taxed by the take) bet rather than a doubly-taxed one. The take-collecting function of the house would persist unchanged; the bias-driven transfer would not.
% FOUNDING_PROBLEM: Odds markets were built to let dispersed, non-expert participants express probability judgments and have a price emerge without requiring anyone to run a formal actuarial model — solving the problem of pricing uncertain events for a mass audience.
% FOUNDING_PROBLEM_CORROBORATION: Academic finance and behavioral economics literature (Ali 1977 onward, Snowberg & Wolfers, and subsequent replications across racing and sports-betting markets) documents the bias from outside the betting industry and outside the syndicate community — this is corroboration from analysts with no stake in either exploiting or defending the public's position; the industry itself does not attest to the bias's severity since acknowledging it would invite disclosure pressure.
narrative_ontology:disappearance_verdict(public_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(public_risk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-25',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(public_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(public_risk_reading, 0.71, 'claude-sonnet-5', 'benter_hkjc_parimutuel_2026_20260825_125025', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.71) reflects a real, replicated, decades-documented transfer from a diffuse, undercapitalized class to a small set of well-modeled agents — this is higher than pure take-extraction because it compounds with the mechanical commission. Suppression is moderate (0.42): there is no active enforcement keeping the public ignorant, but there is a structural information asymmetry — building a calibrated probability model requires resources, data, and statistical training the public bettor does not have and has little practical route to acquire. Accessibility collapse (0.62) is elevated because once a bettor understands the bias exists, the alternative (self-correcting toward calibrated probabilities) is theoretically available but practically inaccessible without the same modeling infrastructure the beneficiaries hold — the alternative is visible but not actionable for most. Resistance is low (0.35): the public rarely organizes against a bias it cannot detect in its own behavior; what little resistance exists comes from analysts publishing the finding, not from bettors changing behavior.
 *
 * DIRECTIONALITY LOGIC:
 *   Public bettors are declared victims: trapped exit (no calibrated alternative readily available at the point of betting), powerless (no capital or modeling infrastructure), and the constraint's effective extraction sits near the full-target end for this seat. Sharp bettors and syndicates are declared beneficiaries: mobile-to-arbitrage exit (can move capital to wherever the divergence is largest), moderate-to-organized power, and directionality sits near the full-beneficiary end. Bookmakers occupy a hybrid position — agenda-setter and secondary beneficiary — because they profit from the take regardless of the bias but structurally benefit from a public that keeps mispricing longshots (it keeps volume flowing into emotionally salient bets). This is why the bookmaker seat is authored as both agenda_setter and beneficiary rather than purely neutral.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function here (aggregating dispersed judgment into a tradeable price) is genuine and would still be needed even if the bias were eliminated — that is why this reading is authored as snare rather than tangled_rope: the coordination function belongs to the ODDS MARKET AS SUCH (which would persist in an unbiased form), not to the BIAS itself. The bias is not solving any coordination problem; it is pure exploitable error riding on top of a coordination mechanism that doesn't require it. This prevents mislabeling the entire odds-market apparatus as extractive (it isn't, in its calibrated form) while still correctly flagging the specific, measurable transfer this reading is about.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    detectability_of_aggregate_bias,
    'Can the favourite-longshot bias be detected and corrected for by an individual bettor with public information alone, or does correction require proprietary modeling infrastructure only sharps and syndicates possess?',
    'Compare outcomes of bettors given simple public-domain bias-correction heuristics (e.g. published de-biasing tables) against bettors given no guidance, across a large sample, to see whether accessible correction meaningfully closes the gap.',
    'If simple public heuristics close most of the gap, accessibility_collapse should be revised downward and the constraint drifts toward rope/tangled_rope (correctable coordination cost); if only proprietary modeling closes it, the snare reading holds and extraction is durable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(detectability_of_aggregate_bias, empirical, 'Whether the bias is correctable with public information or requires proprietary infrastructure.').

omega_variable(
    kernel_reading_disambiguation,
    'Is ''the take is beatable'' best understood as a claim about the mechanical commission (flow_extraction_reading), an unbeatable natural regularity (folk_mountain_reading), a contest against other sophisticated bettors (meta_prediction_reading), or an invisible bias the public carries (this reading) — and does the ordinary bettor''s actual experience match this reading''s framing more than the others?',
    'Survey bettor self-reports of perceived risk source (the house, luck, other bettors, or their own probability judgment) against measured outcome data to see which framing best predicts behavior and losses.',
    'If bettors'' losses correlate most strongly with measurable favourite-longshot mispricing rather than with take size alone, this reading is the empirically dominant one for policy purposes; if losses track take size irrespective of bias, flow_extraction_reading dominates and this reading is a secondary contributor.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Which sibling reading of the beatability-of-the-take kernel best matches observed bettor experience and loss patterns.').

omega_variable(
    syndicate_capture_of_bias_correction,
    'Would public disclosure of the favourite-longshot bias (e.g. regulator-mandated bias-adjusted odds displays) meaningfully close the gap, or would sharps and syndicates simply adapt their models to whatever residual bias remains, preserving most of the extraction?',
    'Study jurisdictions or platforms that have introduced bias-adjusted probability displays and measure whether the sharp/syndicate edge shrinks or merely shifts to a subtler residual bias.',
    'If disclosure meaningfully shrinks the edge, fixing_cost may be closer to cheap than prohibitive and the constraint is more tractable than authored; if the edge simply relocates, the prohibitive fixing_cost and stable extraction pattern are confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(syndicate_capture_of_bias_correction, empirical, 'Whether regulatory disclosure of the bias would durably reduce extraction or merely displace it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_risk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_risk_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(publ_tr_t8, public_risk_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(publ_tr_t16, public_risk_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(publ_tr_t24, public_risk_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(publ_tr_t32, public_risk_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(publ_tr_t40, public_risk_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_risk_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(publ_be_t8, public_risk_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(publ_be_t16, public_risk_reading, base_extractiveness, 16, 0.64).
narrative_ontology:measurement(publ_be_t24, public_risk_reading, base_extractiveness, 24, 0.67).
narrative_ontology:measurement(publ_be_t32, public_risk_reading, base_extractiveness, 32, 0.69).
narrative_ontology:measurement(publ_be_t40, public_risk_reading, base_extractiveness, 40, 0.71).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(public_risk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_risk_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(public_risk_reading, 0.12).
narrative_ontology:affects_constraint(public_risk_reading, flow_extraction_reading).
narrative_ontology:affects_constraint(public_risk_reading, folk_mountain_reading).
narrative_ontology:affects_constraint(public_risk_reading, meta_prediction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the beatability_of_the_take kernel, each instantiating a structurally distinct claim under the colloquial label 'can the take be beaten.' flow_extraction_reading isolates the mechanical commission as the extractive object, independent of any bias. folk_mountain_reading treats house edge as brute natural fact with no identifiable beneficiary structure. meta_prediction_reading frames the relevant adversary as other sophisticated bettors rather than the public. This reading (public_risk_reading) isolates the public's own aggregate probability-estimation error as the primary, largely undisclosed and undetectable risk the public carries — distinguishing it from take-extraction (disclosed, survivable) and from the meta-prediction contest (which presumes bettors are already reasonably calibrated and competing against each other rather than against their own systematic bias).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
