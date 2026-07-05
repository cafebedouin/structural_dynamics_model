% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__real_options_technologist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Real-Options Valuation Legitimacy for Vertically-Integrated Aerospace Portfolio
 *   domain: corporate_finance/technology_governance/space_economics
 *
 * SUMMARY:
 *   This story instantiates the real_options_technologist reading of the
 *   valuation_legitimacy kernel as applied to a vertically-integrated
 *   aerospace and infrastructure company. Under this reading, the company's
 *   ~$1.75T valuation is legitimate because it correctly prices a portfolio
 *   of compounding real options — Starlink (proven, cash-generative),
 *   Starship (high-variance, enabling), orbital compute (unproven but
 *   addressing a genuine 62 GW U.S. power gap), lunar economy (speculative,
 *   first-mover), and Mars (civilizational hedge) — where success in any one
 *   segment raises the probability of success in the others via shared
 *   vertical infrastructure. The reading treats the ~6% implied probability
 *   of reaching a $28.5T total addressable market as a defensible
 *   expected-value calculation, not fantasy. This is deliberately NOT the
 *   dcf_fundamentalist reading (which would treat unproven segments as
 *   worthless options rather than priced assets), NOT the musk_cult_believer
 *   reading (which grounds legitimacy in track record rather than option
 *   mathematics), and NOT the governance_skeptic reading (which treats the
 *   concentrated voting control as the dispositive fact regardless of
 *   valuation methodology). Each of those is a separate constraint story with
 *   its own ε; this file is one reading only.
 *
 * KEY AGENTS:
 *   - spacex_common_shareholders: primary beneficiary class, constrained exit via thin secondary markets
 *   - elon_musk: agenda_setter with concentrated voting control (82.4% votes / 42% equity) who shapes both capital allocation and the valuation narrative
 *   - late_stage_secondary_market_buyers: primary bearer of re-rating risk if the option-pricing framework loses market acceptance
 *   - aspirational_multiplanetary_public: nominal ultimate beneficiary with no financial stake or governance voice
 *   - independent_valuation_analysts: analytical observer seat attempting third-party verification of probability weights
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__real_options_technologist, 0.28).
domain_priors:suppression_score(valuation_legitimacy__real_options_technologist, 0.22).
domain_priors:theater_ratio(valuation_legitimacy__real_options_technologist, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, extractiveness, 0.28).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__real_options_technologist, rope).
narrative_ontology:human_readable(valuation_legitimacy__real_options_technologist, "Real-Options Valuation Legitimacy for Vertically-Integrated Aerospace Portfolio").
narrative_ontology:topic_domain(valuation_legitimacy__real_options_technologist, "corporate_finance/technology_governance/space_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__real_options_technologist, 'c077960d-3928-458a-9a26-e2fb07dcec81').
narrative_ontology:cs_kernel_codification('c077960d-3928-458a-9a26-e2fb07dcec81', distributed).
narrative_ontology:cs_authority_grounding('c077960d-3928-458a-9a26-e2fb07dcec81', distributed).
narrative_ontology:cs_reading_relation('c077960d-3928-458a-9a26-e2fb07dcec81', valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('c077960d-3928-458a-9a26-e2fb07dcec81', valuation_legitimacy__musk_cult_believer, influences).
narrative_ontology:cs_reading_relation('c077960d-3928-458a-9a26-e2fb07dcec81', valuation_legitimacy__governance_skeptic, coexists_with).
narrative_ontology:cs_axiom('c077960d-3928-458a-9a26-e2fb07dcec81', foundational, option_value_is_priceable_present_value).
narrative_ontology:cs_axiom_status(option_value_is_priceable_present_value, holdable).
narrative_ontology:cs_axiom_grounding('c077960d-3928-458a-9a26-e2fb07dcec81', option_value_is_priceable_present_value, instrumental).
narrative_ontology:cs_axiom('c077960d-3928-458a-9a26-e2fb07dcec81', secondary, vertical_integration_compounds_conversion_probability).
narrative_ontology:cs_axiom_status(vertical_integration_compounds_conversion_probability, holdable).
narrative_ontology:cs_axiom_grounding('c077960d-3928-458a-9a26-e2fb07dcec81', vertical_integration_compounds_conversion_probability, empirically_contingent).
narrative_ontology:cs_reference_frame('c077960d-3928-458a-9a26-e2fb07dcec81', real_options_pricing_orthodoxy).
narrative_ontology:cs_drift_state('c077960d-3928-458a-9a26-e2fb07dcec81', post_ipo_speculation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c077960d-3928-458a-9a26-e2fb07dcec81', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__real_options_technologist, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, spacex_common_shareholders).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, spacex_employees_with_equity).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, early_venture_investors).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, aspirational_multiplanetary_public).
narrative_ontology:constraint_victim(valuation_legitimacy__real_options_technologist, late_stage_secondary_market_buyers).
narrative_ontology:constraint_vindicates(valuation_legitimacy__real_options_technologist, vertical_integration_compounds_optionality).
narrative_ontology:constraint_vindicates(valuation_legitimacy__real_options_technologist, real_options_pricing_dominates_dcf_for_platform_bets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold equity priced on a real-options model rather than discounted cash flow from proven segments alone. Benefit if any option (orbital compute, lunar economy, Mars) converts, since vertical integration means a win in one domain raises the modeled probability of wins elsewhere. Secondary-market liquidity is thin and infrequent, so their exit is real but slow and price-discovery-poor.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, spacex_common_shareholders, beneficiary,
    organized, generational, constrained, global).

% Compensated substantially in equity valued under the same option-space framework; their financial outcomes are directly tied to whether the market continues to accept optionality-based pricing over segment-by-segment cash flow discounting.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, spacex_employees_with_equity, beneficiary,
    moderate, biographical, constrained, national).

% Entered at valuations set before Starlink cash flows existed, when the option-space thesis was the only available valuation logic. Have captured enormous markups as Starlink de-risked and reduced their remaining exposure to the speculative tail options; some can sell in structured secondaries at favorable terms unavailable to newer entrants.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, early_venture_investors, beneficiary,
    powerful, generational, arbitrage, global).

% Buy in at a $1.75T valuation that already prices roughly 6% probability of a ~$28.5T total addressable market across the full option portfolio. If the market re-rates toward a DCF-only view of proven segments (Starlink), or if the unproven options fail to convert, they bear the largest mark-to-model losses since they paid closest to the fully-optioned price with the least illiquidity discount.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, late_stage_secondary_market_buyers, payer,
    powerful, biographical, trapped, global).

% Sets the narrative and capital-allocation sequencing across Starlink, Starship, orbital compute, lunar, and Mars programs. Holds 82.4% voting control on ~42% equity, so decisions about which options to fund and how to frame their probability of conversion are made unilaterally, and the option-space valuation story is one he actively promotes and partially controls the evidentiary inputs to.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, elon_musk, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Does not hold equity or bear direct financial risk but is named as the ultimate beneficiary if the civilizational-hedge option (Mars, multiplanetary redundancy) converts. Their 'benefit' is speculative, non-transferable, and has no mechanism for realization independent of the company's private capital decisions.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, aspirational_multiplanetary_public, beneficiary,
    powerless, civilizational, analytical, universal).

% Attempt to price the portfolio's option value independently of company-supplied probability estimates, using comparable technology-option pricing and reference-class forecasting. Have no access to segment-level financials for the unproven programs and must rely largely on disclosed aggregate figures.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, independent_valuation_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__real_options_technologist, early_venture_investors).
narrative_ontology:fixing_cost_class(valuation_legitimacy__real_options_technologist, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a valuation language that lets capital markets price a portfolio of interdependent, unevenly-mature technology bets as a single coherent asset, rather than forcing investors to value each segment (Starlink, Starship, orbital compute, lunar, Mars) in isolation using a framework (DCF) built for mature single-line cash flows.
% TRANSFER_FUNCTION: Moves capital from later-entering investors — who buy at valuations that embed compounding-optionality premia across immature segments — toward earlier investors and equity-compensated insiders who entered before those premia were priced in, converting narrative and technological progress into realized markups for the earlier cohort.
% ABSENT_VOICES: Dissenting valuation methodologists (the dcf_fundamentalist reading) and minority-governance advocates (the governance_skeptic reading) are structurally present in public discourse but are not decision-makers inside the company; late secondary buyers who accept the option-space framing at the point of maximum embedded optimism have no seat in how probability weights are set.
% DISAPPEARANCE_RATIONALE: If the real-options valuation logic were rejected by capital markets overnight in favor of strict DCF on proven cash flows only, the ~$1.75T valuation would likely re-rate sharply downward toward a multiple of Starlink's proven EBITDA, compressing employee and late-investor equity value, altering capital-raise terms for Starship and orbital compute, and removing the financing premium that currently subsidizes speculative programs like Mars.
% FOUNDING_PROBLEM: Traditional discounted-cash-flow valuation cannot price a vertically-integrated technology portfolio where segments are structurally interdependent (Starship's success unlocks orbital compute and lunar economics) and where the highest-value segments have no cash flows yet — a genuine valuation-methodology gap for platform-style compounding-option businesses.
% FOUNDING_PROBLEM_CORROBORATION: Real-options pricing theory itself is corroborated by academic finance literature predating this company (Black-Scholes-Merton lineage, McDonald/Siegel real options work) as a legitimate response to a genuine problem with DCF for high-optionality assets. Independent valuation analysts and some institutional investors outside the company's insider circle affirm the methodological gap is real; governance-skeptic and DCF-fundamentalist critics, also outside the beneficiary set, contend the specific probability weights applied to this portfolio are supplied and controlled by the company itself and cannot be independently verified.
narrative_ontology:disappearance_verdict(valuation_legitimacy__real_options_technologist, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__real_options_technologist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__real_options_technologist, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(valuation_legitimacy__real_options_technologist, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__real_options_technologist, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low-moderate (0.28) because the coordination function is genuine — real-options pricing is an established, academically grounded response to a real methodological problem (pricing interdependent, immature technology segments), and most investors entering at various stages have reasonable information about the speculative nature of the unproven segments. It rises modestly over the interval as the valuation gap between proven cash flow (Starlink's $7.2B EBITDA) and the full $1.75T price grows, meaning a larger share of the valuation rests on unverified probability weights supplied primarily by the company itself. Suppression is low (0.22): no one is coerced into buying equity, and the DCF-fundamentalist and governance-skeptic critiques circulate freely and are not suppressed. Theater ratio rises over time (0.20 to 0.40) reflecting a genuine dynamic: as the achieved and proven portion of the business plateaus relative to the speculative tail, an increasing share of valuation-supporting narrative activity (event announcements, roadmap reveals, timeline framing) serves to sustain the option-pricing story rather than to convey new verifiable information. Accessibility collapse is moderate (0.35): alternative valuation framings (DCF, governance-first) remain fully articulable and are actively argued by named critics; the option-space framing has not foreclosed them, it competes with them. Resistance is moderate (0.40): institutional short-sellers, DCF-oriented analysts, and governance advocates actively contest the framing in public markets and commentary.
 *
 * DIRECTIONALITY LOGIC:
 *   Early venture investors and current common shareholders sit near the beneficiary end: they hold the upside of option conversion and, for the earliest cohort, entered before the current probability premium was priced in. Late secondary buyers sit closer to the target end: they pay a price that already embeds the compounding-optionality premium at its most mature (most expensive) point, and if the framework loses credibility or an option fails to convert, they absorb the largest mark-to-model loss with the least offsetting illiquidity discount already captured. Musk, as agenda_setter with concentrated voting control, sits at a distinct structural position from ordinary beneficiaries: he does not merely benefit from the valuation framework, he actively produces the inputs (timelines, probability framing, program sequencing) that the framework's legitimacy depends on — this is a case where beneficiary and agenda_setter roles are held by the same seat but the roles are analytically distinct (see secondary_role note; not applied here since Musk's primary structural role for this constraint is agenda_setter, not beneficiary, since his benefit is not equity-return-shaped in the same way as ordinary shareholders but control-shaped).
 *
 * MANDATROPHY ANALYSIS:
 *   The real-options valuation logic does not appear mandatrophic under this reading: the founding problem (DCF's inability to price interdependent immature technology segments) remains live as a genuine methodological gap, not a vestigial justification for an arrangement that has outlived its purpose. The contested element is not whether the framework once solved a real problem, but whether the specific probability weights currently applied are independently verifiable or company-supplied — that is routed to an omega rather than treated as settled here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    probability_weight_provenance,
    'Are the implied probability weights (e.g., ~6% chance of reaching the full $28.5T TAM) independently derivable from disclosed technical and market data, or are they effectively supplied and controlled by the company and its founder with no external verification path?',
    'Comparison of independent analyst probability models (built from disclosed launch cadence, contract backlogs, and publicly known technical milestones) against the implied market-priced probability; a persistent, unexplained gap would suggest the weights are narrative-driven rather than data-derived.',
    'If independently derivable, the real-options reading is well-grounded and extraction is genuinely low. If the weights are effectively unfalsifiable narrative inputs supplied by an interested party, the coordination story functions partly as cover for a valuation premium that benefits insiders disproportionately, pushing this constraint toward a tangled_rope reading rather than a clean rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(probability_weight_provenance, empirical, 'Whether the option-pricing probability inputs are independently verifiable or company-controlled.').

omega_variable(
    kernel_reading_coexistence_or_displacement,
    'Do the real_options_technologist, dcf_fundamentalist, musk_cult_believer, and governance_skeptic readings genuinely coexist as live alternative framings held by different market participants, or does the dominance of one reading in practice (e.g., whichever reading the marginal price-setting investor holds) functionally displace the others regardless of their continued rhetorical presence?',
    'Track which reading''s logic best predicts realized price movements around news events (technical milestones vs. governance disclosures vs. cash-flow reports) over multiple cycles; the reading whose logic best predicts marginal price action is the operative one regardless of stated investor beliefs.',
    'If the real-options reading is genuinely the operative marginal-pricing logic, this constraint''s classification as low-extraction rope is well-supported. If a different reading (e.g., musk_cult_believer) actually drives marginal pricing while real-options language is used as post-hoc justification, the coordination function claimed here is partly cosmetic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_coexistence_or_displacement, conceptual, 'Whether the four kernel readings genuinely coexist or one functionally dominates price formation while others provide cover narrative.').

omega_variable(
    civilizational_beneficiary_realizability,
    'Is the ''humanity if multiplanetary civilization succeeds'' beneficiary class a realizable structural beneficiary of this valuation constraint, or a rhetorical beneficiary with no mechanism connecting the valuation framework''s legitimacy to any actual benefit accruing to non-shareholders?',
    'Assess whether any binding commitment (contractual, governance, or legal) ties financial success under this valuation logic to broad public benefit, versus the benefit being contingent, discretionary, and controlled entirely by private equity holders and the controlling shareholder.',
    'If no binding mechanism exists, the public-beneficiary framing functions primarily as legitimacy narrative for investor-facing valuation rather than a structural feature of the constraint, which would lower confidence in treating ''humanity'' as a genuine beneficiary class for directionality purposes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilizational_beneficiary_realizability, conceptual, 'Whether the civilizational beneficiary claim is structurally binding or purely rhetorical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__real_options_technologist, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t0, valuation_legitimacy__real_options_technologist, theater_ratio, 0, 0.2).
narrative_ontology:measurement(valu_tr_t4, valuation_legitimacy__real_options_technologist, theater_ratio, 4, 0.25).
narrative_ontology:measurement(valu_tr_t8, valuation_legitimacy__real_options_technologist, theater_ratio, 8, 0.3).
narrative_ontology:measurement(valu_tr_t12, valuation_legitimacy__real_options_technologist, theater_ratio, 12, 0.34).
narrative_ontology:measurement(valu_tr_t16, valuation_legitimacy__real_options_technologist, theater_ratio, 16, 0.37).
narrative_ontology:measurement(valu_tr_t20, valuation_legitimacy__real_options_technologist, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(valu_be_t0, valuation_legitimacy__real_options_technologist, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(valu_be_t4, valuation_legitimacy__real_options_technologist, base_extractiveness, 4, 0.18).
narrative_ontology:measurement(valu_be_t8, valuation_legitimacy__real_options_technologist, base_extractiveness, 8, 0.21).
narrative_ontology:measurement(valu_be_t12, valuation_legitimacy__real_options_technologist, base_extractiveness, 12, 0.24).
narrative_ontology:measurement(valu_be_t16, valuation_legitimacy__real_options_technologist, base_extractiveness, 16, 0.26).
narrative_ontology:measurement(valu_be_t20, valuation_legitimacy__real_options_technologist, base_extractiveness, 20, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(valuation_legitimacy__real_options_technologist, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__real_options_technologist, resource_allocation).
narrative_ontology:boltzmann_floor_override(valuation_legitimacy__real_options_technologist, 0.12).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__governance_skeptic).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__musk_cult_believer).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the valuation_legitimacy kernel, each instantiating a structurally distinct legitimacy claim about the same company's valuation. The dcf_fundamentalist reading treats unproven segments as pure options with zero present-tense asset value (much lower authored ε for the proven-only core, but implicitly treats the current market price as substantially unjustified premium). The governance_skeptic reading treats the concentrated 82.4%/42% voting-to-equity split as the dispositive extractive fact regardless of valuation methodology (expect higher authored extractiveness and suppression, with minority shareholders as explicit victims). The musk_cult_believer reading grounds legitimacy in founder track record rather than option mathematics (expect different beneficiary/victim framing centered on narrative credibility rather than portfolio pricing). This story's real-options framing is the most methodologically conventional of the four and authors the lowest extractiveness, consistent with it being the reading with the strongest independent academic grounding (real options theory) and the least reliance on unfalsifiable founder-specific claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
