% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__dcf_fundamentalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_valuation_legitimacy__dcf_fundamentalist, []).

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
 *   constraint_id: valuation_legitimacy__dcf_fundamentalist
 *   human_readable: DCF Fundamentalist Reading of Valuation Legitimacy (SpaceX/Musk-adjacent enterprise valuation)
 *   domain: corporate_finance/technology_governance/space_economics
 *
 * SUMMARY:
 *   This story authors the DCF-fundamentalist reading of a contested
 *   valuation kernel applied to a vertically-integrated space/AI enterprise.
 *   Under this reading, legitimate valuation is derived exclusively from
 *   discounting proven, realizable cash flows; unproven ventures (orbital AI
 *   compute, Mars colonization infrastructure) are real options with
 *   option-like present value, not present assets to be capitalized at
 *   revenue multiples appropriate to established businesses. Applied to
 *   disclosed financials — $18.7B revenue, $4.9B net loss — a $1.75T
 *   valuation (93x revenue, negative earnings) is read as fundamentally
 *   unjustifiable by cash-flow standards; only the profitable
 *   satellite-internet segment (~$4.4B operating profit) would support a
 *   valuation in the $44-88B range at conventional 10-20x earnings multiples.
 *   The gap between the fundamentalist-supportable value and the
 *   traded/marked value is, on this reading, a transfer mechanism: insiders
 *   and early investors realize gains denominated at the inflated mark while
 *   public and late-stage investors absorb the eventual repricing risk.
 *
 * KEY AGENTS:
 *   - controlling_founder_musk: agenda_setter/beneficiary (institutional/arbitrage) — sets narrative and disclosure cadence, times personal and insider liquidity events at elevated marks
 *   - early_venture_investors: beneficiary (organized/arbitrage) — realize gains at narrative-driven marks far above cost basis
 *   - public_market_investors: payer (powerless/constrained) — bear the valuation-correction risk implied by the DCF gap
 *   - employee_equity_holders_forced_marks: payer (powerless/trapped) — taxed and compensated at marks they cannot liquidate at parity
 *   - sell_side_analysts_dcf_practitioners: observer (moderate/analytical) — publish the fundamentalist counter-valuation without power to compel repricing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__dcf_fundamentalist, 0.81).
domain_priors:suppression_score(valuation_legitimacy__dcf_fundamentalist, 0.62).
domain_priors:theater_ratio(valuation_legitimacy__dcf_fundamentalist, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, extractiveness, 0.81).
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__dcf_fundamentalist, tangled_rope).
narrative_ontology:human_readable(valuation_legitimacy__dcf_fundamentalist, "DCF Fundamentalist Reading of Valuation Legitimacy (SpaceX/Musk-adjacent enterprise valuation)").
narrative_ontology:topic_domain(valuation_legitimacy__dcf_fundamentalist, "corporate_finance/technology_governance/space_economics").

domain_priors:requires_active_enforcement(valuation_legitimacy__dcf_fundamentalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__dcf_fundamentalist, 'd29e25c1-42b8-4472-bb2c-289b8b15b89f').
narrative_ontology:cs_kernel_codification('d29e25c1-42b8-4472-bb2c-289b8b15b89f', distributed).
narrative_ontology:cs_authority_grounding('d29e25c1-42b8-4472-bb2c-289b8b15b89f', expertise).
narrative_ontology:cs_interpretation_layer_present('d29e25c1-42b8-4472-bb2c-289b8b15b89f').
narrative_ontology:cs_reading_relation('d29e25c1-42b8-4472-bb2c-289b8b15b89f', valuation_legitimacy__real_options_technologist, coexists_with).
narrative_ontology:cs_reading_relation('d29e25c1-42b8-4472-bb2c-289b8b15b89f', valuation_legitimacy__musk_cult_believer, coexists_with).
narrative_ontology:cs_reading_relation('d29e25c1-42b8-4472-bb2c-289b8b15b89f', valuation_legitimacy__governance_skeptic, influences).
narrative_ontology:cs_axiom('d29e25c1-42b8-4472-bb2c-289b8b15b89f', foundational, unrealized_cash_flow_confers_no_present_asset_value).
narrative_ontology:cs_axiom_status(unrealized_cash_flow_confers_no_present_asset_value, holdable).
narrative_ontology:cs_axiom_grounding('d29e25c1-42b8-4472-bb2c-289b8b15b89f', unrealized_cash_flow_confers_no_present_asset_value, empirically_contingent).
narrative_ontology:cs_axiom('d29e25c1-42b8-4472-bb2c-289b8b15b89f', secondary, narrative_momentum_is_not_a_valid_valuation_input).
narrative_ontology:cs_axiom_status(narrative_momentum_is_not_a_valid_valuation_input, holdable).
narrative_ontology:cs_axiom_grounding('d29e25c1-42b8-4472-bb2c-289b8b15b89f', narrative_momentum_is_not_a_valid_valuation_input, conventional).
narrative_ontology:cs_reference_frame('d29e25c1-42b8-4472-bb2c-289b8b15b89f', discounted_cash_flow_orthodoxy).
narrative_ontology:cs_drift_state('d29e25c1-42b8-4472-bb2c-289b8b15b89f', post_zirp_narrative_valuation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d29e25c1-42b8-4472-bb2c-289b8b15b89f', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__dcf_fundamentalist, controlling_founder_musk).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__dcf_fundamentalist, early_venture_investors).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__dcf_fundamentalist, insider_tender_participants).
narrative_ontology:constraint_victim(valuation_legitimacy__dcf_fundamentalist, public_market_investors).
narrative_ontology:constraint_victim(valuation_legitimacy__dcf_fundamentalist, late_stage_secondary_buyers).
narrative_ontology:constraint_victim(valuation_legitimacy__dcf_fundamentalist, employee_equity_holders_forced_marks).
narrative_ontology:constraint_vindicates(valuation_legitimacy__dcf_fundamentalist, discounted_cash_flow_primacy_doctrine).
narrative_ontology:constraint_vindicates(valuation_legitimacy__dcf_fundamentalist, revenue_multiple_discipline_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the narrative frame for what the enterprise is worth by controlling disclosure timing, tender offer pricing, and the mix of press releases about Mars/AI/robotics that anchor investor expectations above what disclosed cash flows support. Holds concentrated voting control and can liquidate personal stakes at valuations the DCF reading says are unsupported by the underlying business.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, controlling_founder_musk, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__dcf_fundamentalist, controlling_founder_musk, beneficiary).

% Bought in at valuations a small fraction of current marks. Sell or mark up their stakes at tender events priced off the speculative narrative, realizing gains that the fundamentalist reading attributes to narrative premium rather than cash-flow growth.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, early_venture_investors, beneficiary,
    organized, biographical, arbitrage, global).

% Executives and select institutional holders participate in periodic internal tender offers priced at the elevated valuation, converting illiquid paper gains to cash before any public market correction could occur.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, insider_tender_participants, beneficiary,
    powerful, immediate, arbitrage, national).

% Buy shares or exposure (directly or through funds) at prices reflecting the 93x-revenue multiple. Under the DCF reading they are paying for cash flows that do not exist and may never materialize at the scale priced in; their exit requires realizing a loss once the market re-prices toward proven-earnings multiples.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, public_market_investors, payer,
    powerless, biographical, constrained, global).

% Purchase private secondary shares near peak marks, often with limited transfer rights and long lockups, leaving them unable to exit quickly if the valuation reading shifts toward the fundamentalist discount.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, late_stage_secondary_buyers, payer,
    moderate, biographical, trapped, national).

% Receive compensation denominated in equity marked at the elevated valuation, are taxed on vesting at that mark, and often cannot sell into a liquid market at the same price — bearing downside risk of a valuation correction without the corresponding upside liquidity the beneficiaries enjoy.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, employee_equity_holders_forced_marks, payer,
    powerless, biographical, trapped, national).

% Publish valuation models applying revenue and earnings multiples from comparable proven businesses (satellite operators, aerospace primes) and flag the gap between disclosed financials and headline valuation, without power to compel repricing.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, sell_side_analysts_dcf_practitioners, observer,
    moderate, immediate, analytical, global).

% Passive fund structures that would hold exposure to the enterprise once it enters relevant indices have no voice in the pricing debate at all — their mandate is to track, not to price — yet their beneficiaries bear the same downside risk as active public investors.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, index_fund_administrators, excluded,
    institutional, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__dcf_fundamentalist, controlling_founder_musk).
narrative_ontology:fixing_cost_class(valuation_legitimacy__dcf_fundamentalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Discounted-cash-flow discipline exists to coordinate capital allocation across an economy: it lets dispersed investors compare businesses on a common yardstick (proven, realizable cash flow) rather than each investor having to independently underwrite every speculative narrative.
% TRANSFER_FUNCTION: Moves capital from investors who accept the elevated, narrative-driven valuation (public market buyers, late secondary buyers, employees marked at peak) to insiders and early investors who can convert paper valuation into realized cash at that price before any correction toward fundamentals.
% ABSENT_VOICES: Index fund administrators and passive retail holders who will be exposed to the valuation through index inclusion have no seat in the pricing conversation; retail buyers evaluating the stock primarily through narrative coverage rather than filings are also structurally absent from the valuation debate despite bearing its downside.
% DISAPPEARANCE_RATIONALE: If DCF discipline as a legitimacy standard disappeared overnight, there would be no counter-narrative anchoring skepticism of the 93x-revenue multiple; capital would flow purely on narrative momentum, insiders could extract at even more extreme multiples, and the eventual correction (when disclosed cash flows fail to catch up) would be larger and more disorderly because no reference frame existed to signal the gap in advance.
% FOUNDING_PROBLEM: DCF-based valuation was built to solve the problem of investors overpaying for speculative promises during recurring bubble cycles (railroads, dot-com, SPACs) by anchoring price to demonstrable, realizable cash generation rather than to story.
% FOUNDING_PROBLEM_CORROBORATION: Independent equity research desks, index providers' own risk disclosures, and prior SEC enforcement actions around SPAC-era projection abuse corroborate that the disclosed-cash-flow-vs-narrative-multiple gap remains a live, recurring problem outside the beneficiary set; no corroboration for the founding problem being 'solved' comes from outside Musk-aligned insiders and early investors themselves.
narrative_ontology:disappearance_verdict(valuation_legitimacy__dcf_fundamentalist, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__dcf_fundamentalist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__dcf_fundamentalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(valuation_legitimacy__dcf_fundamentalist, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__dcf_fundamentalist, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(valuation_legitimacy__dcf_fundamentalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(valuation_legitimacy__dcf_fundamentalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(valuation_legitimacy__dcf_fundamentalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81) and rising over the interval because, under this reading, the gap between fundamentals-supportable value and traded value widens as the narrative components (orbital AI, Mars) receive increasing capitalized value in public discourse and internal tender pricing without corresponding cash-flow realization — this is a rent extracted from later entrants by earlier ones through the mechanism of valuation itself. Suppression (0.62) reflects that alternative pricing information (independent DCF models) exists and circulates, but retail access to primary financial disclosure is asymmetric relative to insider access to forward guidance, and dissenting sell-side views carry real career and access costs (analyst access to management is a lever insiders control). Theater ratio (0.58, rising) captures that an increasing share of the valuation discourse — quarterly narrative updates on Mars timelines, AI compute roadmaps — functions as valuation-maintenance performance rather than disclosure of realizable cash flow; the performative share of investor communication is growing relative to the audited-financials share. Accessibility collapse is moderate (0.45): the DCF counter-model remains fully articulable and public (unlike a true mountain), so alternatives to accepting the elevated valuation are not foreclosed, only costly to act on (shorting carries idiosyncratic risk against a narrative-driven stock). Resistance is substantial (0.68): short sellers, skeptical analysts, and some institutional allocators actively contest the valuation, which is precisely the friction a pure coordination mechanism would not generate.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (controlling founder, early investors, insider tender participants) sit near the full-beneficiary end of directionality: they set or benefit from the pricing frame and hold arbitrage-grade exit, converting paper gains to cash on their own schedule. Victims (public market investors, late-stage secondary buyers, employee equity holders) sit near the full-target end: they are price-takers on a valuation set by others, with constrained or trapped exit options, and bear the downside if the DCF reading's implied correction occurs. The asymmetry is structural, not incidental — the same disclosure and narrative-timing apparatus that lets insiders exit at peak is what leaves public investors holding the position when growth fails to catch up to the multiple.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than a pure snare) reflects that DCF discipline itself performs a genuine, still-live coordination function — it is the yardstick that lets dispersed capital allocators compare businesses without independently underwriting every narrative, and its absence would produce a worse outcome (larger, less-signaled bubbles), not a better one. The extraction is not in DCF discipline itself but in the valuation practice that nominally defers to fundamentals while the actual pricing mechanism (tender offers, index inclusion, insider liquidity events) runs on narrative momentum decoupled from the disclosed cash flows the discipline is supposed to anchor. Classifying this as tangled_rope rather than snare prevents mislabeling the coordination function (comparability, capital discipline) as pure theater, while still registering that an identifiable victim class bears costs an identifiable beneficiary class collects — via the same valuation structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proven_vs_speculative_boundary,
    'Where exactly does ''proven cash flow'' end and ''speculative option value'' begin for a vertically-integrated enterprise where the profitable segment (satellite internet) shares infrastructure, capital, and management attention with the unproven segments (orbital AI, Mars logistics)?',
    'Segment-level audited financial disclosure separating capital allocation, revenue, and cost basis by business line, subject to independent audit rather than management-selected reporting boundaries.',
    'If segments cannot be cleanly separated, the fundamentalist reading''s clean $44-88B floor estimate is itself contestable, narrowing (but not eliminating) the authored extraction gap; if cleanly separable, the gap is closer to the story''s authored 0.81.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proven_vs_speculative_boundary, empirical, 'Whether the profitable and speculative segments can be cleanly valued apart from each other.').

omega_variable(
    dcf_kernel_reading_selection,
    'Is the DCF-fundamentalist reading the structurally correct lens for valuing a founder-controlled, vertically-integrated frontier-technology enterprise, or does the real-options reading better capture legitimate value in a business whose stated purpose is compounding technological optionality?',
    'This is the kernel-level disagreement itself, not resolvable within this story; it is resolved (per party) by which valuation tradition an investor, regulator, or court treats as authoritative. Track realized outcomes over a multi-year horizon: if the option-space investments (Mars, orbital AI) convert to cash flow at a pace and scale consistent with option-value pricing, the real-options reading gains empirical support; if they remain perpetually pre-revenue while the multiple persists, the fundamentalist reading is vindicated.',
    'Resolution in favor of the fundamentalist reading over time would support regulatory or index-inclusion action anchored to disclosed financials; resolution toward the real-options reading would validate the higher multiple as forward-looking rather than extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dcf_kernel_reading_selection, conceptual, 'Committer-level ambiguity: which valuation kernel reading is structurally authoritative for this enterprise type.').

omega_variable(
    insider_timing_intent_ambiguity,
    'Are insider tender offers and liquidity events timed opportunistically relative to narrative peaks (extractive), or are they routine, pre-scheduled liquidity mechanisms that happen to coincide with narrative-driven pricing (non-extractive by design)?',
    'Disclosure of tender offer scheduling history and advance notice periods relative to major narrative announcements (product reveals, timeline updates); comparison against a pre-committed liquidity calendar if one exists.',
    'If timing is opportunistic and discretionary, the beneficiary/victim asymmetry authored here is stronger; if mechanically scheduled and narrative-blind, part of the authored extraction should be attributed to structural liquidity design rather than intentional timing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(insider_timing_intent_ambiguity, empirical, 'Whether insider liquidity timing is opportunistic or mechanically scheduled.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__dcf_fundamentalist, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t0, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 0, 0.32).
narrative_ontology:measurement(valu_tr_t4, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 4, 0.38).
narrative_ontology:measurement(valu_tr_t8, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 8, 0.44).
narrative_ontology:measurement(valu_tr_t12, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 12, 0.49).
narrative_ontology:measurement(valu_tr_t16, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 16, 0.53).
narrative_ontology:measurement(valu_tr_t20, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 20, 0.56).
narrative_ontology:measurement(valu_tr_t24, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 24, 0.58).

% Extraction over time
narrative_ontology:measurement(valu_be_t0, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(valu_be_t4, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 4, 0.61).
narrative_ontology:measurement(valu_be_t8, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(valu_be_t12, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 12, 0.73).
narrative_ontology:measurement(valu_be_t16, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 16, 0.77).
narrative_ontology:measurement(valu_be_t20, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 20, 0.79).
narrative_ontology:measurement(valu_be_t24, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 24, 0.81).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(valuation_legitimacy__dcf_fundamentalist, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__dcf_fundamentalist, resource_allocation).
narrative_ontology:boltzmann_floor_override(valuation_legitimacy__dcf_fundamentalist, 0.12).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy__real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy__musk_cult_believer).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy__governance_skeptic).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposed from the single natural-language label 'valuation legitimacy' per the kernel/reading framework: each reading of the valuation_legitimacy kernel (dcf_fundamentalist, real_options_technologist, musk_cult_believer, governance_skeptic) instantiates a structurally distinct constraint with its own extraction profile, beneficiary/victim set, and classification. This file authors only the dcf_fundamentalist reading; the sibling files author the others independently, and none of the four should be read as measuring the 'same' constraint from different angles — per the ε-invariance principle, differing ε values across these readings mean they are different constraints, linked here for network/contamination-propagation purposes only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
