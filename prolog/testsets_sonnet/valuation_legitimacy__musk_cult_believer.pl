% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__musk_cult_believer
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_valuation_legitimacy__musk_cult_believer, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: valuation_legitimacy__musk_cult_believer
 *   human_readable: Track-Record Valuation Legitimacy (Musk Execution-History Reading)
 *   domain: corporate_finance/technology_governance/space_economics
 *
 * SUMMARY:
 *   This story instantiates the 'musk_cult_believer' reading of the
 *   valuation_legitimacy kernel: the claim that legitimacy for a founder-led
 *   technology conglomerate's market valuation derives from a demonstrated
 *   history of achieving goals the market previously priced as impossible
 *   (orbital-class reusable rockets, satellite-internet profitability, EV
 *   manufacturing scale), and that trailing financial metrics (revenue
 *   multiples, debt coverage, near-term cash flow) are lagging indicators
 *   that should be systematically discounted relative to the forward-looking
 *   execution thesis. Statements characterizing 'genuine risk of bankruptcy'
 *   are read under this frame as negotiating leverage deployed in labor or
 *   supplier disputes, not as sincere risk disclosures. A
 *   multi-billion-dollar performance-vesting compensation package tied to
 *   milestones including sustained off-world presence is read as credible
 *   commitment mechanism rather than speculative fantasy, because the track
 *   record is taken to license extrapolation. This is one of four sibling
 *   readings of the same kernel (dcf_fundamentalist, governance_skeptic,
 *   real_options_technologist); each is authored as its own ε-invariant
 *   constraint per DP-001, linked via network.affects_constraints. The claim
 *   (tangled_rope) and the metrics are authored independently: the
 *   coordination function (funding genuinely novel engineering that
 *   fundamentals-pricing would starve) is real, and the extraction (short
 *   squeezes, analyst reputational punishment, minority-shareholder dilution
 *   funding a founder-legitimacy narrative) is also real and requires active
 *   narrative and legal maintenance to sustain.
 *
 * KEY AGENTS:
 *   - musk_personal_equity_position: primary agenda_setter/beneficiary (institutional/arbitrage) — controls and profits from the narrative
 *   - long_term_retail_believers: beneficiary (moderate/mobile) — gains from narrative-driven appreciation
 *   - tesla_board_aligned_insiders: agenda_setter/beneficiary (organized/constrained) — approves and defends the framing
 *   - short_sellers: primary target (powerful/trapped) — structurally punished by narrative persistence regardless of fundamentals
 *   - skeptical_analysts: target (moderate/constrained) — professionally penalized for fundamentals-based dissent
 *   - minority_shareholders_diluted_by_comp_package: target (powerless/constrained) — bears dilution cost of the legitimacy mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__musk_cult_believer, 0.62).
domain_priors:suppression_score(valuation_legitimacy__musk_cult_believer, 0.58).
domain_priors:theater_ratio(valuation_legitimacy__musk_cult_believer, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, extractiveness, 0.62).
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__musk_cult_believer, tangled_rope).
narrative_ontology:human_readable(valuation_legitimacy__musk_cult_believer, "Track-Record Valuation Legitimacy (Musk Execution-History Reading)").
narrative_ontology:topic_domain(valuation_legitimacy__musk_cult_believer, "corporate_finance/technology_governance/space_economics").

domain_priors:requires_active_enforcement(valuation_legitimacy__musk_cult_believer).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__musk_cult_believer, '0aef928c-fb8d-4540-942c-f528fd4949fa').
narrative_ontology:cs_kernel_codification('0aef928c-fb8d-4540-942c-f528fd4949fa', distributed).
narrative_ontology:cs_authority_grounding('0aef928c-fb8d-4540-942c-f528fd4949fa', distributed).
narrative_ontology:cs_reading_relation('0aef928c-fb8d-4540-942c-f528fd4949fa', valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('0aef928c-fb8d-4540-942c-f528fd4949fa', valuation_legitimacy__real_options_technologist, influences).
narrative_ontology:cs_reading_relation('0aef928c-fb8d-4540-942c-f528fd4949fa', valuation_legitimacy__governance_skeptic, forecloses).
narrative_ontology:cs_axiom('0aef928c-fb8d-4540-942c-f528fd4949fa', foundational, execution_history_supersedes_trailing_financials).
narrative_ontology:cs_axiom_status(execution_history_supersedes_trailing_financials, holdable).
narrative_ontology:cs_axiom_grounding('0aef928c-fb8d-4540-942c-f528fd4949fa', execution_history_supersedes_trailing_financials, instrumental).
narrative_ontology:cs_axiom('0aef928c-fb8d-4540-942c-f528fd4949fa', foundational, founder_capability_moots_governance_concentration_risk).
narrative_ontology:cs_axiom_status(founder_capability_moots_governance_concentration_risk, holdable).
narrative_ontology:cs_axiom_grounding('0aef928c-fb8d-4540-942c-f528fd4949fa', founder_capability_moots_governance_concentration_risk, conventional).
narrative_ontology:cs_reference_frame('0aef928c-fb8d-4540-942c-f528fd4949fa', post_reusable_rocket_vindication_baseline).
narrative_ontology:cs_drift_state('0aef928c-fb8d-4540-942c-f528fd4949fa', post_2024_milestone_slippage_pattern, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0aef928c-fb8d-4540-942c-f528fd4949fa', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__musk_cult_believer, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__musk_cult_believer, long_term_retail_believers).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__musk_cult_believer, musk_personal_equity_position).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__musk_cult_believer, tesla_board_aligned_insiders).
narrative_ontology:constraint_victim(valuation_legitimacy__musk_cult_believer, short_sellers).
narrative_ontology:constraint_victim(valuation_legitimacy__musk_cult_believer, skeptical_analysts).
narrative_ontology:constraint_victim(valuation_legitimacy__musk_cult_believer, minority_shareholders_diluted_by_comp_package).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the narrative that valuation should be read against a personal execution history (reusable rockets, Starlink, EV scale) rather than current financials. Sets investor-relations framing, timing of announcements, and the terms under which skepticism is characterized as failure to understand the vision. Holds a compensation package (including the disputed billion-share performance grant) whose value depends entirely on this framing being accepted by markets and courts.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, musk_personal_equity_position, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__musk_cult_believer, musk_personal_equity_position, beneficiary).

% Buy and hold on the thesis that Musk's track record of delivering 'impossible' engineering feats is the correct discount mechanism for future cash flows. Benefit when the stock appreciates on narrative momentum rather than earnings; can exit anytime by selling, but exiting means abandoning the belief structure that justified the purchase price in the first place.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, long_term_retail_believers, beneficiary,
    moderate, generational, mobile, global).

% Approve and defend the compensation structure and public statements that tie corporate legitimacy to founder mythology. Their own board seats and reputations are bound to the valuation narrative holding; approving the pay package and defending it in litigation is their primary lever.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, tesla_board_aligned_insiders, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__musk_cult_believer, tesla_board_aligned_insiders, agenda_setter).

% Bet against the valuation on the grounds that financial fundamentals cannot support the price; the track-record narrative structurally overwhelms fundamentals-based positions and margin calls or short squeezes force capitulation before models are vindicated. 'Genuine risk of bankruptcy' claims made in one period and reversed in the next are read by this seat as negotiating tactics that punish anyone who took them at face value.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, short_sellers, payer,
    powerful, immediate, trapped, global).

% Publish valuation models based on discounted cash flow or comparable-company analysis and are professionally penalized (client attrition, reputational cost, being 'proven wrong' by price action) when the stock continues to appreciate on narrative rather than fundamentals. Cannot exit the disagreement without either capitulating to the narrative or losing institutional standing.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, skeptical_analysts, payer,
    moderate, biographical, constrained, national).

% Hold shares diluted by the scale of the performance-vesting compensation package; their claim on the company's value is structurally subordinated to the mechanism that sustains the founder-legitimacy narrative. Exit means selling at whatever price the narrative currently supports, which is itself shaped by the constraint they are trying to escape.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, minority_shareholders_diluted_by_comp_package, payer,
    powerless, generational, constrained, global).

% The sibling reading holding that 82.4% voting control concentrated in one person is extraction regardless of track record. Not admitted into this reading's frame of legitimacy — under the track-record view, governance concerns are treated as irrelevant noise when the founder is judged uniquely capable, so this position has no seat at the table it would need to be heard from.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, governance_skeptic_reading, excluded,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(valuation_legitimacy__musk_cult_believer, governance_skeptic_reading).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__musk_cult_believer, musk_personal_equity_position).
narrative_ontology:fixing_cost_class(valuation_legitimacy__musk_cult_believer, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates capital allocation toward high-risk, long-horizon engineering bets (reusable rockets, satellite constellations, autonomous vehicles) that conventional DCF-based capital markets would systematically underfund, by substituting a track-record heuristic for near-term cash-flow proof.
% TRANSFER_FUNCTION: Moves capital from positions betting against the narrative (short sellers, skeptical analysts who sized positions against fundamentals) and from diluted minority shareholders toward long-term believers and toward the founder's personal equity and compensation position, mediated by price appreciation sustained by the execution-history story rather than current earnings.
% ABSENT_VOICES: The governance_skeptic reading is structurally excluded from this frame — under a track-record legitimacy standard, concentrated voting control and compensation scale are treated as non-issues because the founder's capability is taken as dispositive; diluted minority shareholders who would raise governance objections have no procedural lever proportionate to their economic exposure.
% DISAPPEARANCE_RATIONALE: If track-record legitimacy stopped functioning as a valuation anchor overnight, the stock would need to reprice toward a fundamentals or real-options basis; the compensation package's Mars-colony vesting condition would lose its narrative cover, short positions would be substantially de-risked, and board members who defended the framing would face renewed governance scrutiny.
% FOUNDING_PROBLEM: Conventional valuation methods (DCF, comparables) systematically undervalue founder-led firms attempting genuinely novel engineering feats where no historical cash-flow base exists to discount, and markets need SOME mechanism to price technologies with no track record except the founder's prior track record in adjacent impossible-seeming projects.
% FOUNDING_PROBLEM_CORROBORATION: Believers and board-aligned insiders attest the founding problem is live — genuinely novel technology cannot be priced by trailing financials. Short sellers, skeptical analysts, and independent forensic accounting commentary (published DCF teardowns, options-market implied-volatility analyses) attest that the 'financial metrics are lagging indicators' framing has become a permanent excuse for un-auditable valuation rather than a temporary bridge past a genuine pricing problem — corroboration exists on both sides from parties with no financial stake in the outcome (academic finance commentary cited by both camps), but no single neutral corroborator has settled which framing is presently operative.
narrative_ontology:disappearance_verdict(valuation_legitimacy__musk_cult_believer, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__musk_cult_believer, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__musk_cult_believer, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(valuation_legitimacy__musk_cult_believer, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__musk_cult_believer, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(valuation_legitimacy__musk_cult_believer_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(valuation_legitimacy__musk_cult_believer, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(valuation_legitimacy__musk_cult_believer_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) and theater_ratio (0.55) are both authored moderately high and rising: the coordination function (funding hard engineering) is genuine at the outset of the interval but has increasingly become entangled with narrative maintenance that outpaces the underlying delivery cadence — later-period announcements and timeline claims show growing gap between promised and delivered milestones, which is the theater component. Suppression (0.58) reflects the active work required to keep the narrative dominant: litigation over the compensation package, public rebuttal of short positions, and reputational pressure on dissenting analysts. Accessibility_collapse is authored moderate-low (0.4) because genuine alternative valuation frames (DCF, real-options) remain visible and actively argued in public markets — this is a contested legitimacy claim, not a fully collapsed one. Resistance is authored high (0.72) because organized, well-resourced opposition (short sellers, institutional skeptics, governance advocates) actively contests the frame rather than merely grumbling.
 *
 * DIRECTIONALITY LOGIC:
 *   musk_personal_equity_position and tesla_board_aligned_insiders sit near the full-beneficiary end: they set the narrative terms and their wealth/position is the direct output of the frame holding. long_term_retail_believers are beneficiaries with mobile exit — they profit from the frame but are not trapped in it, they chose in. short_sellers sit near full-target: trapped exit options (margin calls, squeeze dynamics force capitulation independent of being right), and the frame's persistence is what extracts from their positions. skeptical_analysts and minority_shareholders are targets with constrained exit: professional and economic costs of dissent or of holding diluted equity are real but not absolute traps.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (novel engineering needs a pricing mechanism that isn't trailing-cash-flow-based) was genuinely live in the early reusable-rocket and early-EV-scaling period. Whether it remains live at the current valuation scale is exactly the contested question this story routes through founding_problem_status: contested. Classifying this as tangled_rope rather than snare prevents mislabeling a partly-genuine coordination mechanism (funding real technological risk-taking) as pure extraction; classifying it as tangled_rope rather than rope prevents treating the narrative's real victims (squeezed shorts, diluted minority holders, penalized analysts) as merely incidental to a clean coordination story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    track_record_extrapolation_validity,
    'Does a demonstrated history of achieving specific ''impossible'' engineering milestones license extrapolation to unrelated future claims (off-world colonization timelines, full autonomy timelines) at the same confidence level, or does each domain require independent evidence?',
    'Track the calibration of past founder-stated timelines against actual delivery dates across multiple ventures; a systematic multi-year slippage pattern would undermine extrapolation validity even where the underlying capability claims eventually proved partly true.',
    'If extrapolation is invalid, the legitimacy mechanism substitutes charisma-driven forecasting for evidence and the constraint is more purely extractive than coordinative; if valid within bounds, the coordination function is more substantial than the metrics currently assume.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(track_record_extrapolation_validity, empirical, 'Whether track record in one domain licenses valuation confidence in unrelated future claims.').

omega_variable(
    bankruptcy_statements_sincerity,
    'Were public statements characterizing existential financial risk to the company sincere risk disclosures at the time made, negotiating leverage deployed for external audiences (labor, suppliers, regulators), or both simultaneously?',
    'Compare contemporaneous internal financial documentation, board minutes, and analyst briefings against the public statements'' timing and audience to assess whether risk framing tracked internal assessment or external negotiating need.',
    'If sincere, the track-record reading''s dismissal of these statements as mere tactics is itself a legitimacy-protecting reinterpretation, deepening rather than resolving the extraction reading; if genuinely tactical, the dismissal is structurally accurate and the reading''s core claim strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bankruptcy_statements_sincerity, empirical, 'Sincerity vs. tactical function of stated existential-risk warnings.').

omega_variable(
    kernel_reading_selection_bias,
    'Is the choice between the four sibling readings of this kernel (musk_cult_believer, dcf_fundamentalist, real_options_technologist, governance_skeptic) itself correlated with the observer''s prior financial exposure (long position, short position, no position), such that the reading functions less as an independent epistemic frame and more as a rationalization of pre-existing economic interest?',
    'Survey which reading is invoked by which class of market participant conditional on disclosed position; strong correlation between position and reading choice would support the rationalization hypothesis over the independent-frame hypothesis.',
    'If reading choice tracks economic position near-perfectly, none of the four readings can claim to be a neutral valuation methodology and the kernel contest itself is better modeled as a proxy fight rather than genuine interpretive disagreement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_bias, conceptual, 'Whether kernel-reading selection is independent analysis or interest-driven rationalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__musk_cult_believer, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t0, valuation_legitimacy__musk_cult_believer, theater_ratio, 0, 0.3).
narrative_ontology:measurement(valu_tr_t4, valuation_legitimacy__musk_cult_believer, theater_ratio, 4, 0.35).
narrative_ontology:measurement(valu_tr_t8, valuation_legitimacy__musk_cult_believer, theater_ratio, 8, 0.4).
narrative_ontology:measurement(valu_tr_t12, valuation_legitimacy__musk_cult_believer, theater_ratio, 12, 0.46).
narrative_ontology:measurement(valu_tr_t16, valuation_legitimacy__musk_cult_believer, theater_ratio, 16, 0.5).
narrative_ontology:measurement(valu_tr_t20, valuation_legitimacy__musk_cult_believer, theater_ratio, 20, 0.53).
narrative_ontology:measurement(valu_tr_t24, valuation_legitimacy__musk_cult_believer, theater_ratio, 24, 0.55).

% Extraction over time
narrative_ontology:measurement(valu_be_t0, valuation_legitimacy__musk_cult_believer, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(valu_be_t4, valuation_legitimacy__musk_cult_believer, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(valu_be_t8, valuation_legitimacy__musk_cult_believer, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(valu_be_t12, valuation_legitimacy__musk_cult_believer, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(valu_be_t16, valuation_legitimacy__musk_cult_believer, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(valu_be_t20, valuation_legitimacy__musk_cult_believer, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(valu_be_t24, valuation_legitimacy__musk_cult_believer, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(valu_su_t0, valuation_legitimacy__musk_cult_believer, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(valu_su_t4, valuation_legitimacy__musk_cult_believer, suppression_requirement, 4, 0.45).
narrative_ontology:measurement(valu_su_t8, valuation_legitimacy__musk_cult_believer, suppression_requirement, 8, 0.49).
narrative_ontology:measurement(valu_su_t12, valuation_legitimacy__musk_cult_believer, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(valu_su_t16, valuation_legitimacy__musk_cult_believer, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(valu_su_t20, valuation_legitimacy__musk_cult_believer, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(valu_su_t24, valuation_legitimacy__musk_cult_believer, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__musk_cult_believer, resource_allocation).
narrative_ontology:boltzmann_floor_override(valuation_legitimacy__musk_cult_believer, 0.12).
narrative_ontology:affects_constraint(valuation_legitimacy__musk_cult_believer, dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__musk_cult_believer, real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__musk_cult_believer, governance_skeptic).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the valuation_legitimacy kernel, each authored as its own ε-invariant constraint. musk_cult_believer treats financial metrics as lagging and founder track record as the primary legitimacy anchor (ε=0.62, tangled_rope). dcf_fundamentalist treats unproven technology as an option rather than an asset and anchors legitimacy in discounted proven cash flow (expected lower ε, closer to a rope/snare boundary depending on discount assumptions). real_options_technologist treats vertical integration as compounding technological optionality (expected moderate ε, closer to rope given genuine coordination framing). governance_skeptic treats concentrated voting control as extraction independent of track record (expected higher ε, closer to snare given explicit victim framing of minority shareholders). All four share the same underlying facts about the company's operations and financial history; they differ in which observable (track record, DCF, option value, governance structure) they treat as legitimacy-determining. Per DP-001, these are decomposed into separate stories rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
