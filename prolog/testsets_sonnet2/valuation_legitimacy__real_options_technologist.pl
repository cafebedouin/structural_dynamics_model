% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__real_options_technologist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: valuation_legitimacy__real_options_technologist
 *   human_readable: Real-Options Valuation of SpaceX's Vertically Integrated Technology Portfolio
 *   domain: Corporate Finance / Technology Governance / Space Economics
 *
 * SUMMARY:
 *   This story instantiates the real-options-technologist reading of the
 *   SpaceX valuation-legitimacy kernel: valuation is legitimate to the extent
 *   it prices the present value of a technological option portfolio (Starlink
 *   proven and cash-generative; Starship high-variance but option-enabling;
 *   orbital compute addressing a genuine 62 GW U.S. power gap; lunar economy
 *   speculative but first-mover; Mars a civilizational hedge), where vertical
 *   integration means success in one segment raises the probability of
 *   success in others. Under this reading the ~$1.75T valuation prices
 *   roughly a 6% probability of the portfolio realizing a $28.5T total
 *   addressable market — a coherent, if aggressive, probability-weighted
 *   framing rather than either a pure cash-flow multiple (the
 *   dcf_fundamentalist reading) or a founder-track-record heuristic (the
 *   musk_cult_believer reading). This reading takes no position on the
 *   governance-control critique (the governance_skeptic reading); it
 *   evaluates only whether the option-portfolio pricing logic itself is a
 *   coherent valuation framework, not whether the control structure that
 *   administers it is legitimate.
 *
 * KEY AGENTS:
 *   - spacex_existing_shareholders: primary beneficiary of option-framed pricing (organized/constrained)
 *   - musk_control_bloc: agenda-setter who allocates capital across the option portfolio (institutional/arbitrage)
 *   - late_round_secondary_market_investors: bear the largest markdown risk if the thesis under-delivers (moderate/trapped)
 *   - starlink_dependent_customers: beneficiaries of the one realized, cash-generative option (powerless/constrained)
 *   - dcf_fundamentalist_analysts and minority_governance_advocates: excluded voices whose objections are structurally absent from the private valuation process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__real_options_technologist, 0.38).
domain_priors:suppression_score(valuation_legitimacy__real_options_technologist, 0.22).
domain_priors:theater_ratio(valuation_legitimacy__real_options_technologist, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, extractiveness, 0.38).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__real_options_technologist, rope).
narrative_ontology:human_readable(valuation_legitimacy__real_options_technologist, "Real-Options Valuation of SpaceX's Vertically Integrated Technology Portfolio").
narrative_ontology:topic_domain(valuation_legitimacy__real_options_technologist, "Corporate Finance / Technology Governance / Space Economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__real_options_technologist, '75d05135-af8b-4b8c-932b-786d2851a5ee').
narrative_ontology:cs_kernel_codification('75d05135-af8b-4b8c-932b-786d2851a5ee', distributed).
narrative_ontology:cs_authority_grounding('75d05135-af8b-4b8c-932b-786d2851a5ee', distributed).
narrative_ontology:cs_reading_relation('75d05135-af8b-4b8c-932b-786d2851a5ee', valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('75d05135-af8b-4b8c-932b-786d2851a5ee', valuation_legitimacy__musk_cult_believer, influences).
narrative_ontology:cs_reading_relation('75d05135-af8b-4b8c-932b-786d2851a5ee', valuation_legitimacy__governance_skeptic, coexists_with).
narrative_ontology:cs_axiom('75d05135-af8b-4b8c-932b-786d2851a5ee', foundational, option_value_is_priceable_present_value).
narrative_ontology:cs_axiom_status(option_value_is_priceable_present_value, holdable).
narrative_ontology:cs_axiom_grounding('75d05135-af8b-4b8c-932b-786d2851a5ee', option_value_is_priceable_present_value, instrumental).
narrative_ontology:cs_axiom('75d05135-af8b-4b8c-932b-786d2851a5ee', foundational, vertical_integration_compounds_cross_segment_probability).
narrative_ontology:cs_axiom_status(vertical_integration_compounds_cross_segment_probability, holdable).
narrative_ontology:cs_axiom_grounding('75d05135-af8b-4b8c-932b-786d2851a5ee', vertical_integration_compounds_cross_segment_probability, empirically_contingent).
narrative_ontology:cs_reference_frame('75d05135-af8b-4b8c-932b-786d2851a5ee', segment_level_dcf_pricing_convention).
narrative_ontology:cs_drift_state('75d05135-af8b-4b8c-932b-786d2851a5ee', post_2024_tender_offer_valuations, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('75d05135-af8b-4b8c-932b-786d2851a5ee', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__real_options_technologist, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, spacex_existing_shareholders).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, spacex_employees_with_equity).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, musk_control_bloc).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, starlink_dependent_customers).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, future_multiplanetary_beneficiaries).
narrative_ontology:constraint_victim(valuation_legitimacy__real_options_technologist, late_round_secondary_market_investors).
narrative_ontology:constraint_vindicates(valuation_legitimacy__real_options_technologist, compounding_optionality_thesis).
narrative_ontology:constraint_vindicates(valuation_legitimacy__real_options_technologist, vertical_integration_increases_cross_segment_success_probability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold equity marked at a valuation built on discounted probability-weighted option value across five business lines rather than current cash flow multiples. They benefit directly if the option framing holds because it prices in upside from Starship, orbital compute, lunar economy, and Mars that a pure-DCF price would not capture; their liquidity is limited to structured tender offers, so they cannot test the market price against public trading.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, spacex_existing_shareholders, beneficiary,
    organized, generational, constrained, global).

% Compensated substantially in equity valued under the same option-portfolio logic. Their financial outcomes are tied to whether the option thesis is later vindicated by Starship's reuse economics and orbital compute demand; they cannot independently hedge or diversify this exposure easily given private-company liquidity constraints.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, spacex_employees_with_equity, beneficiary,
    moderate, biographical, constrained, national).

% Sets capital allocation across the option portfolio (Starlink cash flow funds Starship development funds orbital compute and lunar bids), and controls the narrative that vertical integration compounds cross-segment probability. Extracts governance control and reputational capital from the option framing being accepted by capital markets; can reallocate resources between segments without minority shareholder veto.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, musk_control_bloc, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__real_options_technologist, musk_control_bloc, beneficiary).

% Buy in at tender-offer prices set by the option-portfolio valuation near the top of the pricing curve. If the ~6% probability-weighted TAM realization thesis proves too optimistic, they absorb the largest markdown with the least ability to exit before repricing, since secondary shares in a private company cannot be sold on demand.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, late_round_secondary_market_investors, payer,
    moderate, biographical, trapped, national).

% Rely on Starlink's proven, cash-generative service, which is the option portfolio's one realized asset. They benefit from continued investment the option-framed valuation makes possible, largely independent of whether the speculative legs of the thesis ever pay off.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, starlink_dependent_customers, beneficiary,
    powerless, biographical, constrained, global).

% A non-actor placeholder for the civilizational-hedge beneficiary class the reading names explicitly (humanity, if multiplanetary settlement succeeds). Named for completeness of the option thesis's own stated payoff structure; collects nothing today and cannot be a party to any present transfer.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, future_multiplanetary_beneficiaries, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(valuation_legitimacy__real_options_technologist, future_multiplanetary_beneficiaries).

% Financial analysts who would price SpaceX on discounted proven cash flow alone (effectively Starlink's EBITDA and near-term contracts) reject the option-portfolio premium as unfalsifiable. They are not represented in the private valuation process, which is negotiated directly between the company and its chosen investors rather than through public price discovery.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, dcf_fundamentalist_analysts, excluded,
    moderate, immediate, analytical, national).

% Would argue that a valuation this dependent on the option thesis holding requires governance protections against unilateral capital reallocation; they have no seat at the table given the control bloc's supermajority voting position and are absent from the reading this constraint instantiates.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, minority_governance_advocates, excluded,
    moderate, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__real_options_technologist, diffuse).
narrative_ontology:fixing_cost_class(valuation_legitimacy__real_options_technologist, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables continued private capital formation across a multi-decade, multi-segment technology roadmap that would be underfunded under conventional single-business DCF pricing, by pricing the whole portfolio's compounding cross-segment option value rather than each segment's isolated proven cash flow.
% TRANSFER_FUNCTION: Moves capital from investors willing to price probability-weighted technological optionality into R&D and infrastructure buildout across Starship, orbital compute, and lunar/Mars programs; moves upside (if realized) from the enterprise back to equity holders and the control bloc in proportion to their stakes, with Starlink's proven cash flow subsidizing the unproven legs in the interim.
% ABSENT_VOICES: DCF fundamentalist analysts and minority-governance advocates would object that the option framing is unfalsifiable and shields capital allocation from independent scrutiny, but neither has standing in a privately negotiated valuation process controlled by the company and its chosen investors.
% DISAPPEARANCE_RATIONALE: If the option-portfolio valuation logic were rejected in favor of pure DCF pricing, the company's private valuation would compress sharply toward Starlink's standalone cash-flow multiple, secondary-market tender pricing would reset lower, employee equity compensation value would fall, and capital allocation toward the more speculative segments (orbital compute, lunar economy, Mars) would face much tighter funding discipline.
% FOUNDING_PROBLEM: Conventional cash-flow valuation cannot price a vertically integrated technology company whose current profitable segment (Starlink) exists partly to fund unproven segments (Starship's next-generation vehicle, orbital compute, lunar and Mars ventures) whose value depends on cross-segment compounding effects that a segment-by-segment DCF would ignore or discount to near zero.
% FOUNDING_PROBLEM_CORROBORATION: The company and its chosen late-stage investors attest the option-portfolio framing correctly prices genuine technological optionality (citing Starlink's realized EBITDA as evidence the thesis has partly paid off). Independent financial analysts applying DCF methodology and governance-focused commentators outside the investor base dispute this, arguing the framing is structurally unfalsifiable and cannot be distinguished from narrative-driven overpricing absent public market price discovery.
narrative_ontology:disappearance_verdict(valuation_legitimacy__real_options_technologist, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__real_options_technologist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__real_options_technologist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(valuation_legitimacy__real_options_technologist, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__real_options_technologist, 0.38, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored moderate-low (0.38 at interval end) because the reading's own claim is that the option portfolio has a real, if uncertain, payoff structure — Starlink's $7.2B EBITDA is not speculative, it is realized, and it substantially anchors the valuation floor. Suppression is low (0.22): no party is coercively prevented from declining to invest, and public information about segment status (launch cadence, Starlink revenue disclosures, orbital compute demand data) is broadly available, though the private-market structure limits price discovery. Theater ratio rises modestly (0.18 to 0.30) reflecting that as segments mature without full realization (particularly Starship's most speculative downstream applications), an increasing share of the valuation narrative may substitute promotional framing for demonstrated option value — this is authored as a mild drift, not a dominant feature, consistent with the reading's own confidence in the thesis.
 *
 * DIRECTIONALITY LOGIC:
 *   Existing shareholders, employees with equity, and the control bloc sit near the beneficiary end: the option framing directly inflates the value of what they hold or control. Late-round secondary investors sit nearer the target end because they buy at prices set by the option thesis at its most optimistic point in the funding cycle, with the least ability to exit before any repricing. Starlink customers benefit incidentally from continued investment without bearing valuation risk. The civilizational-hedge beneficiary class is declared non-agent because it collects nothing in the present and cannot be a party to any current transfer — it is named only because the reading's own stated payoff structure explicitly invokes it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that conventional DCF cannot price cross-segment compounding optionality in a vertically integrated technology company — remains genuinely live as long as Starship, orbital compute, and lunar/Mars segments remain unresolved; this is not a Piton because the coordination function (funding continued technological development that a narrower valuation method would starve) has not atrophied into pure performance. The story's low authored suppression and moderate extraction keep it clear of Snare or Tangled Rope territory under this reading's own metrics, even though the governance_skeptic sibling reading would authors a very different extraction and suppression profile for the same underlying arrangement viewed through the control-concentration lens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    option_thesis_falsifiability,
    'Is the compounding-optionality thesis (success in one segment raising probability of success in others) an empirically testable claim, or is it structured so that any outcome can be retrofitted to confirm it?',
    'Track whether Starship''s launch cadence and reuse economics measurably correlate with subsequent orbital compute and lunar contract wins over a multi-year window; a null or negative correlation would falsify the compounding claim as authored.',
    'If falsified, the option-portfolio valuation premium collapses toward a DCF-style segment-by-segment price, and this reading''s claimed_type and extraction metrics would need substantial revision toward the dcf_fundamentalist reading''s territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(option_thesis_falsifiability, empirical, 'Whether cross-segment compounding optionality is a testable claim or an unfalsifiable narrative device.').

omega_variable(
    sibling_reading_divergence_source,
    'The four kernel readings (real_options_technologist, dcf_fundamentalist, musk_cult_believer, governance_skeptic) disagree sharply on extraction and legitimacy for what is nominally the same valuation event. Is this because they disagree about facts, or because they are answering structurally different questions (pricing methodology vs. founder credibility vs. governance structure)?',
    'Decompose each reading''s claim into its testable factual component and its normative-framing component; readings that share factual premises but diverge only in framing are evidence the kernel genuinely supports multiple non-foreclosing readings rather than one being simply mistaken.',
    'If readings diverge primarily on framing rather than fact, coexists_with relations are correctly assigned across the kernel and no single reading should be treated as dominant by the engine; if a factual dispute is discovered, one reading may in fact foreclose another.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_divergence_source, conceptual, 'Whether the kernel''s sibling readings differ on facts or only on evaluative framing of shared facts.').

omega_variable(
    civilizational_beneficiary_discounting,
    'How should a civilizational-hedge beneficiary (a multiplanetary human future) that exists only as a probability-weighted, multi-decade-out payoff be treated in a present valuation exercise — as a legitimate component of option value, or as a rhetorical device with no proper discount rate?',
    'Compare analogous long-horizon option valuations in other industries (e.g., pharmaceutical platform valuations pricing decades-out cures) to establish whether comparable discounting conventions exist and are applied consistently here.',
    'If no defensible discounting convention exists for this component, its inclusion in the option-portfolio valuation is closer to narrative premium than priced option value, which would raise the reading''s authored extraction figure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilizational_beneficiary_discounting, conceptual, 'Whether civilizational-scale, multi-decade payoffs can be legitimately discounted into present valuation or function as unfalsifiable narrative premium.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__real_options_technologist, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t0, valuation_legitimacy__real_options_technologist, theater_ratio, 0, 0.18).
narrative_ontology:measurement(valu_tr_t4, valuation_legitimacy__real_options_technologist, theater_ratio, 4, 0.2).
narrative_ontology:measurement(valu_tr_t8, valuation_legitimacy__real_options_technologist, theater_ratio, 8, 0.23).
narrative_ontology:measurement(valu_tr_t12, valuation_legitimacy__real_options_technologist, theater_ratio, 12, 0.26).
narrative_ontology:measurement(valu_tr_t16, valuation_legitimacy__real_options_technologist, theater_ratio, 16, 0.28).
narrative_ontology:measurement(valu_tr_t20, valuation_legitimacy__real_options_technologist, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(valu_be_t0, valuation_legitimacy__real_options_technologist, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(valu_be_t4, valuation_legitimacy__real_options_technologist, base_extractiveness, 4, 0.26).
narrative_ontology:measurement(valu_be_t8, valuation_legitimacy__real_options_technologist, base_extractiveness, 8, 0.3).
narrative_ontology:measurement(valu_be_t12, valuation_legitimacy__real_options_technologist, base_extractiveness, 12, 0.33).
narrative_ontology:measurement(valu_be_t16, valuation_legitimacy__real_options_technologist, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(valu_be_t20, valuation_legitimacy__real_options_technologist, base_extractiveness, 20, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(valuation_legitimacy__real_options_technologist, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__real_options_technologist, resource_allocation).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__musk_cult_believer).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__governance_skeptic).

% DUAL FORMULATION NOTE:
% This story is one of four readings of the valuation_legitimacy kernel applied to SpaceX. Each reading is authored as a separate ε-invariant constraint per the decomposition principle: dcf_fundamentalist prices only proven cash flow and treats the rest as unpriced options (much lower authored extraction, much higher accessibility_collapse against alternative framings); musk_cult_believer substitutes founder track record for financial metrics as the legitimating evidence (different beneficiary/victim structure, higher authored theater_ratio); governance_skeptic evaluates the control-concentration structure independent of pricing methodology (much higher authored extraction and suppression, naming minority shareholders as victims). All four are linked bidirectionally as siblings in the same kernel contest; none is treated as authoritative over the others by this file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
