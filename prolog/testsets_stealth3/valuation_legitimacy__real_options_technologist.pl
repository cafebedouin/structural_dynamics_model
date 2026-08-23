% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__real_options_technologist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_valuation_legitimacy__real_options_technologist, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: valuation_legitimacy__real_options_technologist
 *   human_readable: Option-Space Valuation Legitimacy Norm (Real-Options Technologist Reading)
 *   domain: economic/financial/technological
 *
 * SUMMARY:
 *   This story instantiates ONE reading — real_options_technologist — of the
 *   contested valuation_legitimacy kernel: the claim that a price is
 *   legitimate when it equals the present value of the technological option
 *   space a venture controls, and that vertical integration compounds that
 *   value by raising joint exercise probability across segments. The standing
 *   arrangement under contest is SpaceX's capital regime, in which successive
 *   private rounds price the company at marks justified by a portfolio of
 *   options: Starlink (proven, $7.2B EBITDA), Starship (high-variance enabler
 *   of all downstream options), orbital compute (unproven but addressing a
 *   genuine 62 GW U.S. power gap), lunar economy (speculative first-mover
 *   claims), and Mars (civilizational hedge). The $1.75T pricing embeds
 *   roughly a 6 percent probability of a $28.5T portfolio-wide TAM. Family
 *   decomposition: the colloquial label 'valuation legitimacy' covers four
 *   structurally distinct claims, instantiated as separate files —
 *   dcf_fundamentalist, governance_skeptic, musk_cult_believer, and this one.
 *   Over the SAME referent, the siblings author different epsilon: the dcf
 *   reading prices most of the premium as fiction, the governance reading
 *   locates extraction in the control structure, the cult reading authors
 *   near-zero extraction. This reading authors 0.38: most of the price is
 *   fair payment for priced risk, with a bounded residue of opacity premium,
 *   tail-risk transfer onto last buyers, and externalities the frame does not
 *   count. KEY AGENTS (by structural relationship): - founder_ceo_office:
 *   agenda setter (institutional/identity_locked) — sets evidence standards
 *   and round anchors; legacy fused with the frame -
 *   spacex_operating_company: primary beneficiary (institutional/arbitrage) —
 *   banks option-priced development capital -
 *   early_venture_investors_and_insiders: secondary beneficiary
 *   (powerful/arbitrage) — realizes gains at insider-scheduled tenders -
 *   late_round_private_market_buyers: primary paying seat among investors
 *   (powerful/constrained) — buys option premiums for illiquid claims -
 *   equity_compensated_engineering_workforce: second paying seat
 *   (moderate/constrained) — trades liquid wages for narrative-dependent
 *   equity - radio_astronomy_community: unseated cost bearer
 *   (organized/trapped) — absorbs constellation externalities with no
 *   valuation channel - dcf_disciplined_asset_allocators: excluded voice
 *   (institutional/trapped) — mandated out of the price-setting room -
 *   independent_aerospace_financial_economists: analytical observer
 *   (analytical/analytical) — tracks option-exercise base rates
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__real_options_technologist, 0.38).
domain_priors:suppression_score(valuation_legitimacy__real_options_technologist, 0.55).
domain_priors:theater_ratio(valuation_legitimacy__real_options_technologist, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, extractiveness, 0.38).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__real_options_technologist, tangled_rope).
narrative_ontology:human_readable(valuation_legitimacy__real_options_technologist, "Option-Space Valuation Legitimacy Norm (Real-Options Technologist Reading)").
narrative_ontology:topic_domain(valuation_legitimacy__real_options_technologist, "economic/financial/technological").

domain_priors:requires_active_enforcement(valuation_legitimacy__real_options_technologist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__real_options_technologist, 'a15f3d24-512d-4144-a1e6-27ff0484b75e').
narrative_ontology:cs_kernel_codification('a15f3d24-512d-4144-a1e6-27ff0484b75e', distributed).
narrative_ontology:cs_authority_grounding('a15f3d24-512d-4144-a1e6-27ff0484b75e', expertise).
narrative_ontology:cs_interpretation_layer_present('a15f3d24-512d-4144-a1e6-27ff0484b75e').
narrative_ontology:cs_reading_relation('a15f3d24-512d-4144-a1e6-27ff0484b75e', valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('a15f3d24-512d-4144-a1e6-27ff0484b75e', valuation_legitimacy__governance_skeptic, coexists_with).
narrative_ontology:cs_reading_relation('a15f3d24-512d-4144-a1e6-27ff0484b75e', valuation_legitimacy__musk_cult_believer, influences).
narrative_ontology:cs_axiom('a15f3d24-512d-4144-a1e6-27ff0484b75e', foundational, capability_option_value_grounded_legitimacy).
narrative_ontology:cs_axiom_status(capability_option_value_grounded_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a15f3d24-512d-4144-a1e6-27ff0484b75e', capability_option_value_grounded_legitimacy, instrumental).
narrative_ontology:cs_axiom('a15f3d24-512d-4144-a1e6-27ff0484b75e', foundational, vertical_integration_compounds_joint_option_probability).
narrative_ontology:cs_axiom_status(vertical_integration_compounds_joint_option_probability, holdable).
narrative_ontology:cs_axiom_grounding('a15f3d24-512d-4144-a1e6-27ff0484b75e', vertical_integration_compounds_joint_option_probability, empirically_contingent).
narrative_ontology:cs_axiom('a15f3d24-512d-4144-a1e6-27ff0484b75e', secondary, dotcom_attention_metrics_discredited_as_options).
narrative_ontology:cs_axiom_status(dotcom_attention_metrics_discredited_as_options, overridden).
narrative_ontology:cs_axiom_grounding('a15f3d24-512d-4144-a1e6-27ff0484b75e', dotcom_attention_metrics_discredited_as_options, empirically_contingent).
narrative_ontology:cs_reference_frame('a15f3d24-512d-4144-a1e6-27ff0484b75e', capability_portfolio_valuation_norm).
narrative_ontology:cs_drift_state('a15f3d24-512d-4144-a1e6-27ff0484b75e', post_rate_shock_ai_capex_revival, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('a15f3d24-512d-4144-a1e6-27ff0484b75e', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__real_options_technologist, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, spacex_operating_company).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, early_venture_investors_and_insiders).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, financial_media_narrators).
narrative_ontology:constraint_victim(valuation_legitimacy__real_options_technologist, late_round_private_market_buyers).
narrative_ontology:constraint_victim(valuation_legitimacy__real_options_technologist, equity_compensated_engineering_workforce).
narrative_ontology:constraint_victim(valuation_legitimacy__real_options_technologist, radio_astronomy_community).
narrative_ontology:constraint_vindicates(valuation_legitimacy__real_options_technologist, real_options_valuation_doctrine).
narrative_ontology:constraint_vindicates(valuation_legitimacy__real_options_technologist, vertical_integration_compounding_thesis).
narrative_ontology:constraint_vindicates(valuation_legitimacy__real_options_technologist, multiplanetary_civilization_hedge_premise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets what counts as evidence (flight tests and hardware demos over audited statements), the cadence of disclosure, and the anchor price for each financing round. Holds roughly 42 percent economic interest alongside supermajority voting control. Personal legacy, public identity, and the multiplanetary project are fused into a single narrative; abandoning the capability-framing would mean repudiating the life's work, so departure from the frame is not a live option from this seat.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, founder_ceo_office, agenda_setter,
    institutional, generational, identity_locked, global).

% Operates launch, satellite broadband, and advanced development programs funded by successive private rounds priced on technological capability rather than current earnings. Each round's full proceeds — including the portion above any cash-flow-justified value — convert into banked development capital for Starship, orbital compute, and lunar programs. Downside exposure is capped by the corporate form: capital received is retained whether or not the priced-in futures arrive.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, spacex_operating_company, beneficiary,
    institutional, generational, arbitrage, global).

% Held positions from rounds priced before the capability narrative matured. Sell portions into company-scheduled tender offers at each upward re-mark, so realized gains compound with every headline valuation. Liquidity timing is set from inside; the risk of any individual tender is the next buyer's.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, early_venture_investors_and_insiders, beneficiary,
    powerful, biographical, arbitrage, global).

% Produce coverage, documentaries, and social amplification built on launch spectacles and valuation milestones. Revenue follows engagement, which peaks with dramatic engineering footage and big-number headlines. No capital is committed to the ventures being covered, and pivoting the narrative carries no cost.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, financial_media_narrators, beneficiary,
    organized, immediate, mobile, continental).

% Sovereign wealth funds, mutual-fund complexes, and crossover vehicles buying positions at marks embedding roughly a 6 percent probability of a $28.5T portfolio-wide outcome. Positions are illiquid between company-scheduled tenders; diligence relies on management-presented milestones rather than independently verifiable cash flows; exiting means finding a later buyer willing to pay a higher narrative.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, late_round_private_market_buyers, payer,
    powerful, biographical, constrained, global).

% Engineers and technical staff accepting below-market cash compensation supplemented by equity that vests on multi-year cliffs. Realized value depends on the valuation narrative holding through each employee's vesting window; resigning forfeits unvested grants; mission prestige and peer culture make remaining the default choice.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, equity_compensated_engineering_workforce, payer,
    moderate, biographical, constrained, global).

% Ground-based observatories and research programs whose optical images are streaked by satellite trains and whose radio bands are crowded by constellation downlinks. Objections filed through FCC and ITU comment processes carry no weight in the funding conversations that set constellation scale; mitigation costs fall on the observatories, and no channel exists through which these costs enter the valuation calculus.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, radio_astronomy_community, payer,
    organized, generational, trapped, global).

% Pension funds, insurers, and index managers mandated to cash-flow-backed assets and barred from private rounds by accreditation rules and mandate language. They would price the venture dramatically lower on observable financials; their absence from the room is what keeps the price-setting conversation closed to their methodology.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, dcf_disciplined_asset_allocators, excluded,
    institutional, biographical, trapped, global).

% Academic and think-tank researchers tracking base rates of venture-backed technology options reaching exercisable outcomes, publishing calibration studies of round-mark inflation and post-money-to-revenue ratios. Hold no positions in the ventures studied; influence is limited to citation.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, independent_aerospace_financial_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__real_options_technologist, spacex_operating_company).
narrative_ontology:fixing_cost_class(valuation_legitimacy__real_options_technologist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the temporal-mismatch problem of funding genuinely valuable multi-decade technology: discounted-cash-flow instruments cannot price assets whose value lies in unrealized capability, so the norm coordinates patient capital around a shared framework for assessing option value, assembling pools large enough to sustain vertically integrated development (launch, broadband, compute, exploration) that no single revenue line could fund.
% TRANSFER_FUNCTION: Moves capital from late-round private-market investors, and via equity-heavy compensation from engineering labor, into the operating company's technology programs; moves narrative authority over pricing from financial-statement analysis to technological-assessment judgment; moves realized gains, when options exercise or rounds re-mark, primarily to early holders selling into tenders.
% ABSENT_VOICES: Radio astronomers bear constellation-scale costs and have filed objections through FCC/ITU channels with no seat in any funding decision. Cash-flow-mandated fiduciaries are barred from the rounds by accreditation and mandate rules and would price the venture far lower. Future orbital users inherit debris-risk accumulation nobody prices. Non-accredited retail investors absorb distorted public-market comparables while being unable to access the private rounds that set them.
% DISAPPEARANCE_RATIONALE: If the norm vanished overnight, every SpaceX program without near-term cash flow loses its funding basis: Starship-class development halts (no cash-flow instrument funds it), orbital compute and lunar programs become unfundable, and Starlink survives on proven EBITDA but at reduced expansion speed. Late-round paper marks deflate sharply, the tender pipeline closes, and the media economy around milestone spectacle contracts. Deployed hardware keeps interfering with astronomy, but the growth of that interference stops.
% FOUNDING_PROBLEM: After the dot-com correction, public-market cash-flow discipline made it nearly impossible to fund decade-long infrastructure bets such as reusable rockets and LEO broadband at scale; the option-space framework was built to legitimize patient capital deployment against capability portfolios rather than current earnings.
% FOUNDING_PROBLEM_CORROBORATION: The general funding-gap problem is corroborated from outside the benefiting parties by the academic real-options literature (the Dixit-Pindyck tradition predates and is independent of any single venture) and by aerospace-industry economists documenting the 2012-2016 capital-structure gap for reusable-launch development. The specific application to current round marks is attested only by round participants; no outside party corroborates that the $1.75T pricing tracks real option value.
narrative_ontology:disappearance_verdict(valuation_legitimacy__real_options_technologist, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__real_options_technologist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__real_options_technologist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(valuation_legitimacy__real_options_technologist, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__real_options_technologist, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(valuation_legitimacy__real_options_technologist_tests).
:- end_tests(valuation_legitimacy__real_options_technologist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.38 because, by this reading's own lights, most of the option premium is a fair price for priced risk — the frame concedes the 94-percent tail openly — but three residues are real: the opacity premium (no continuous price discovery disciplines the mark), the tail transfer (whoever buys last funds earlier holders' realized gains when options lapse), and the unpriced externalities (astronomy, debris, spectrum). Suppression is 0.55 and is authored as a RAW structural property — the engine scales only extractiveness: private-market opacity, insider-controlled tender timing, accreditation barriers excluding cash-flow-mandated capital, and narrative dismissal of cash-flow critique ('failure of imagination'). Theater ratio 0.30: the underlying engineering is real (reuse works, Starlink earns), but a growing share of communications activity serves narrative maintenance — booster-catch spectacle, Mars renders, milestone countdowns timed to fundraising windows. Accessibility_collapse is 0.65, not mountain-grade: once inside the frame, the alternative (pricing SpaceX on cash flows) collapses as incoherent, but sibling frames remain live outside it, which is precisely why resistance persists at 0.45 (short-seller campaigns, governance challenges, the 2022 rate-shock repricing pressure, astronomer petitions). The measurement series run on ONE shared six-point grid (2016-2026, all three metrics at every point, endpoint values matching the scalars). Trajectory arc: 2016 propulsive-landing proof gives the frame credibility; 2020-era zero-rate liquidity escalates round marks; the 2022 rate shock shows up as the steepest suppression_requirement step (0.43 to 0.49) as the frame defends itself against duration repricing; 2024-2026 AI-capex and orbital-compute enthusiasm revive the frame, with the 2026 endpoints authored as projected.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical artifacts. The founder-office seat experiences the regime as the only honest way to fund civilizational infrastructure — any cash-flow discipline reads from inside as civilizational surrender. Late-round buyers experience it as high-stakes illiquid faith whose only exit is recruiting the next believer. Engineers experience prestige braided with golden handcuffs: the mission is real, and so is the cliff. Astronomers experience pure cost with no channel into the conversation that scales it. Media narrators experience a content windfall with zero capital at risk. Nothing in the artifact itself adjudicates among these; the engine computes per-seat types from the structural data (power, exit, role), and the divergence between the agenda-setter's experience and the payer seats' experience is the measured quantity.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries cluster at the low-d end: the operating company banks proceeds with arbitrage-grade downside protection (capital received is retained regardless of option exercise), insiders exit through windows they schedule, and media carriers hold no capital at risk with mobile exit. Payers cluster at the high-d end: late-round buyers are powerful but constrained (illiquidity between tenders), the workforce is moderately powered but cliff-bound, and the astronomy community is the highest-effective-extraction seat in the story — organized but outmatched, exit impossible (the sky cannot be relocated), and completely unweighted in the pricing process. Suppression being unscaled means the trapped, lower-power seats feel the frame's full coercive force while the powerful constrained seats negotiate around it. One directionality override is authored for the institutional power atom (d=0.15): the derivation chain has no beneficiary/victim declaration for the agenda-setter seat (he is not a clean beneficiary group), so the canonical institutional fallback would misplace him; 42 percent economic skin in the game plus total narrative control justify placing him near the beneficiary pole, and the override modestly adjusts the operating-company seat in the same direction without changing its sign.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification guards both symmetric errors. Reading the regime as a pure snare erases the genuine coordination: the patient-capital channel this norm opened funded reusable launch, mega-constellation broadband, and a launch-cost collapse that no cash-flow instrument would have financed, and the founding problem (cash-flow instruments structurally cannot price capability portfolios) is still live, corroborated by the independent real-options literature. Reading it as a pure rope erases the extraction: the tail transfer onto last buyers, the opacity premium, the identity-locked consolidation of narrative authority, and the unseated externalities. Keeping coordination function and extraction on the same ledger is the point of the hybrid. No mandatrophy is declared — the founding problem has not died — but the drift indicators to watch are both rising in the series: theater growth toward proxy-substitution territory and enforcement-intensification of the narrative machinery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Is the real_options_technologist reading the correct instantiation of the valuation_legitimacy kernel for this arrangement, or does a sibling reading (dcf_fundamentalist, governance_skeptic, musk_cult_believer) better match how the regime actually operates?',
    'Cross-reading corpus comparison: which reading''s epsilon and stakeholder structure best predicts realized round behavior, tender outcomes, write-downs, and governance events.',
    'Sibling instantiations carry different victim sets and epsilon over the same referent: if the governance_skeptic reading fits better, extraction concentrates in the control structure and this file understates it; if dcf_fundamentalist fits, the premium is mispricing rather than extraction; if the cult reading fits, the frame is person-dependent and collapses with the founder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Which reading of the valuation-legitimacy kernel this arrangement actually instantiates.').

omega_variable(
    tam_probability_calibration,
    'Is the roughly 6 percent implied probability of realizing the $28.5T portfolio TAM calibrated, or inflated by narrative feedback between milestone spectacle and round pricing?',
    'Base-rate studies of vertically integrated venture portfolios, prediction-market pricing of the constituent outcomes, and ex-post audits of option-exercise frequencies.',
    'If the true probability is well below 6 percent, the overpayment transferred from late buyers exceeds what the frame concedes and effective extraction rises toward the snare boundary for last entrants; if calibrated or conservative, the authored epsilon stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tam_probability_calibration, empirical, 'Whether the priced-in portfolio success probability is statistically honest.').

omega_variable(
    cross_segment_coupling_sign,
    'Does vertical integration genuinely create positive cross-segment probability coupling (success anywhere lifts all options), or does it concentrate correlated failure modes (a single launch-cadence, capital-continuity, or key-person failure impairing every option simultaneously)?',
    'Covariance analysis of segment outcomes across historical stress events (launch failures, funding pauses, regulatory shocks); counterfactual comparison against spun-out single-segment peers.',
    'If coupling is negative or correlated-failure-dominated, the reading''s distinguishing axiom collapses, the frame dissolves into generic option pricing, and effective epsilon rises; positive coupling supports the authored structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cross_segment_coupling_sign, empirical, 'Sign and strength of the vertical-integration compounding premise.').

omega_variable(
    late_buyer_consent_depth,
    'Are late-round buyers genuinely informed risk-takers consenting to the 94-percent tail, or are they fiduciary-herding into opacity, unable to independently verify the milestones that justify each re-mark?',
    'LP-side diligence records, allocation-committee minutes, and structured interviews after write-down events.',
    'Shallow consent widens the victim set beyond the frame''s own accounting and raises effective extraction above the authored epsilon; deep consent confirms the low-victim delta this reading claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(late_buyer_consent_depth, empirical, 'Depth of the consent that keeps the victim set small.').

omega_variable(
    externality_seating_gap,
    'How large are the costs borne by parties with no seat in the valuation process (astronomy interference, orbital debris risk, spectrum crowding), and would counting them as victims materially change the classification?',
    'Monetize the externality stream against option value; reprice the arrangement counterfactually with mitigation costs internalized.',
    'Material externalities add victims the frame does not count, raising effective extraction for excluded and unseated cost bearers and pulling the arrangement toward heavier extraction than this reading concedes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_seating_gap, empirical, 'Size of the unseated cost-bearer set the frame leaves out of its victim accounting.').

omega_variable(
    workforce_retention_mechanism_split,
    'How much of the engineering workforce''s constrained exit is structural (vesting cliffs, forfeiture of unvested grants) versus internalized (mission identity, peer culture)?',
    'Post-vest-window attrition analysis and exit interviews of engineers who walked away from outstanding grants.',
    'If internalized retention dominates, effective suppression exceeds the structural measure and travels with departing engineers; if structural, the authored suppression stands and expires with vesting completion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(workforce_retention_mechanism_split, empirical, 'Structural versus internalized share of workforce exit costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__real_options_technologist, 2016, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t2016, valuation_legitimacy__real_options_technologist, theater_ratio, 2016, 0.16).
narrative_ontology:measurement_basis(valu_tr_t2016, observed).
narrative_ontology:measurement(valu_tr_t2018, valuation_legitimacy__real_options_technologist, theater_ratio, 2018, 0.19).
narrative_ontology:measurement_basis(valu_tr_t2018, observed).
narrative_ontology:measurement(valu_tr_t2020, valuation_legitimacy__real_options_technologist, theater_ratio, 2020, 0.23).
narrative_ontology:measurement_basis(valu_tr_t2020, observed).
narrative_ontology:measurement(valu_tr_t2022, valuation_legitimacy__real_options_technologist, theater_ratio, 2022, 0.26).
narrative_ontology:measurement_basis(valu_tr_t2022, observed).
narrative_ontology:measurement(valu_tr_t2024, valuation_legitimacy__real_options_technologist, theater_ratio, 2024, 0.29).
narrative_ontology:measurement_basis(valu_tr_t2024, observed).
narrative_ontology:measurement(valu_tr_t2026, valuation_legitimacy__real_options_technologist, theater_ratio, 2026, 0.3).
narrative_ontology:measurement_basis(valu_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(valu_be_t2016, valuation_legitimacy__real_options_technologist, base_extractiveness, 2016, 0.2).
narrative_ontology:measurement_basis(valu_be_t2016, observed).
narrative_ontology:measurement(valu_be_t2018, valuation_legitimacy__real_options_technologist, base_extractiveness, 2018, 0.24).
narrative_ontology:measurement_basis(valu_be_t2018, observed).
narrative_ontology:measurement(valu_be_t2020, valuation_legitimacy__real_options_technologist, base_extractiveness, 2020, 0.29).
narrative_ontology:measurement_basis(valu_be_t2020, observed).
narrative_ontology:measurement(valu_be_t2022, valuation_legitimacy__real_options_technologist, base_extractiveness, 2022, 0.33).
narrative_ontology:measurement_basis(valu_be_t2022, observed).
narrative_ontology:measurement(valu_be_t2024, valuation_legitimacy__real_options_technologist, base_extractiveness, 2024, 0.36).
narrative_ontology:measurement_basis(valu_be_t2024, observed).
narrative_ontology:measurement(valu_be_t2026, valuation_legitimacy__real_options_technologist, base_extractiveness, 2026, 0.38).
narrative_ontology:measurement_basis(valu_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(valu_su_t2016, valuation_legitimacy__real_options_technologist, suppression_requirement, 2016, 0.32).
narrative_ontology:measurement_basis(valu_su_t2016, observed).
narrative_ontology:measurement(valu_su_t2018, valuation_legitimacy__real_options_technologist, suppression_requirement, 2018, 0.38).
narrative_ontology:measurement_basis(valu_su_t2018, observed).
narrative_ontology:measurement(valu_su_t2020, valuation_legitimacy__real_options_technologist, suppression_requirement, 2020, 0.43).
narrative_ontology:measurement_basis(valu_su_t2020, observed).
narrative_ontology:measurement(valu_su_t2022, valuation_legitimacy__real_options_technologist, suppression_requirement, 2022, 0.49).
narrative_ontology:measurement_basis(valu_su_t2022, observed).
narrative_ontology:measurement(valu_su_t2024, valuation_legitimacy__real_options_technologist, suppression_requirement, 2024, 0.53).
narrative_ontology:measurement_basis(valu_su_t2024, observed).
narrative_ontology:measurement(valu_su_t2026, valuation_legitimacy__real_options_technologist, suppression_requirement, 2026, 0.55).
narrative_ontology:measurement_basis(valu_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__real_options_technologist, resource_allocation).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__governance_skeptic).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__musk_cult_believer).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'valuation legitimacy' decomposes into four structurally distinct claims per the epsilon-invariance principle — measuring legitimacy by cash-flow discounting versus capability-option pricing versus founder-track-record versus governance protection yields different epsilon, different victim sets, and different failure modes over the same referent. This reading sits mid-family: it inherits the dcf_fundamentalist's challenge (its premium is only as good as its exercise base rates) and supplies the evidential substrate (demonstrated milestones) that the musk_cult_believer reading metabolizes. All four files cross-link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(valuation_legitimacy__real_options_technologist, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
