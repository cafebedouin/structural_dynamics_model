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
 *   human_readable: Option-Space Valuation Legitimacy Regime (Real-Options Technologist Reading)
 *   domain: corporate finance / technology governance / space economics
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the valuation_legitimacy kernel:
 *   the real_options_technologist reading, under which a valuation is
 *   legitimate when it equals the present value of the technological option
 *   space it opens, and vertical integration compounds optionality across
 *   segments. As instantiated in the standing arrangement under contest, the
 *   frame prices a five-segment portfolio - Starlink (proven, ~$7.2B EBITDA),
 *   Starship (high-variance enabler of all downstream options), orbital
 *   compute (unproven, aimed at a genuine 62 GW U.S. power gap), lunar
 *   economy (speculative first-mover), Mars (civilizational hedge) - at
 *   $1.75T, implying roughly a 6% probability weight on a $28.5T portfolio
 *   TAM. The epsilon referent is the standing arrangement itself (the
 *   option-space pricing regime as actually practiced: seller-set tender
 *   marks, selective disclosure, no public filings), assessed by this
 *   reading's own lights - hence a moderate epsilon even from a seat
 *   sympathetic to the frame. The frame performs real coordination (a
 *   workable pricing language for unpriceable assets) while concentrating
 *   pricing power and control benefits in one seat. KEY AGENTS (by structural
 *   relationship): - musk_control_block: agenda setter and primary
 *   beneficiary (institutional / identity_locked) - sets tender prices,
 *   controls disclosure, collects the control premium -
 *   early_stage_investors: primary beneficiary (powerful / mobile) - paper
 *   gains compound with each upward mark - late_stage_tender_participants:
 *   primary target (institutional / constrained) - buys the full mark with
 *   illiquid shares and thin information rights - employee_equity_holders:
 *   dual target-beneficiary (moderate / trapped) - equity-weighted pay binds
 *   savings and career to one vehicle - ai_compute_buyers: contingent
 *   beneficiary (organized / mobile) - their 62 GW demand forecast is the
 *   input that prices the orbital-compute option - humanity_option_holder:
 *   declared conditional beneficiary (non-agent in present time) - collects
 *   only in the multiplanetary-success branch -
 *   independent_dcf_valuation_community: excluded voice (institutional /
 *   analytical) - publishes rival prices no filing regime forces anyone to
 *   answer - aerospace_finance_observers: analytical observer (institutional
 *   / analytical) - tracks option conversion rates from outside the tenders
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__real_options_technologist, 0.34).
domain_priors:suppression_score(valuation_legitimacy__real_options_technologist, 0.31).
domain_priors:theater_ratio(valuation_legitimacy__real_options_technologist, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, extractiveness, 0.34).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__real_options_technologist, tangled_rope).
narrative_ontology:human_readable(valuation_legitimacy__real_options_technologist, "Option-Space Valuation Legitimacy Regime (Real-Options Technologist Reading)").
narrative_ontology:topic_domain(valuation_legitimacy__real_options_technologist, "corporate finance / technology governance / space economics").

domain_priors:requires_active_enforcement(valuation_legitimacy__real_options_technologist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__real_options_technologist, 'c9f9868b-a450-4b10-861c-2429eb3db996').
narrative_ontology:cs_kernel_codification('c9f9868b-a450-4b10-861c-2429eb3db996', distributed).
narrative_ontology:cs_authority_grounding('c9f9868b-a450-4b10-861c-2429eb3db996', expertise).
narrative_ontology:cs_interpretation_layer_present('c9f9868b-a450-4b10-861c-2429eb3db996').
narrative_ontology:cs_reading_relation('c9f9868b-a450-4b10-861c-2429eb3db996', valuation_legitimacy__dcf_fundamentalist, influences).
narrative_ontology:cs_reading_relation('c9f9868b-a450-4b10-861c-2429eb3db996', valuation_legitimacy__musk_cult_believer, coexists_with).
narrative_ontology:cs_reading_relation('c9f9868b-a450-4b10-861c-2429eb3db996', valuation_legitimacy__governance_skeptic, coexists_with).
narrative_ontology:cs_axiom('c9f9868b-a450-4b10-861c-2429eb3db996', foundational, option_space_present_value_grounds_legitimacy).
narrative_ontology:cs_axiom_status(option_space_present_value_grounds_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('c9f9868b-a450-4b10-861c-2429eb3db996', option_space_present_value_grounds_legitimacy, instrumental).
narrative_ontology:cs_axiom('c9f9868b-a450-4b10-861c-2429eb3db996', foundational, vertical_integration_compounds_optionality).
narrative_ontology:cs_axiom_status(vertical_integration_compounds_optionality, holdable).
narrative_ontology:cs_axiom_grounding('c9f9868b-a450-4b10-861c-2429eb3db996', vertical_integration_compounds_optionality, empirically_contingent).
narrative_ontology:cs_reference_frame('c9f9868b-a450-4b10-861c-2429eb3db996', technological_option_space_primacy).
narrative_ontology:cs_drift_state('c9f9868b-a450-4b10-861c-2429eb3db996', starlink_cashflow_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('c9f9868b-a450-4b10-861c-2429eb3db996', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__real_options_technologist, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, musk_control_block).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, early_stage_investors).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, employee_equity_holders).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, ai_compute_buyers).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, humanity_option_holder).
narrative_ontology:constraint_victim(valuation_legitimacy__real_options_technologist, late_stage_tender_participants).
narrative_ontology:constraint_victim(valuation_legitimacy__real_options_technologist, employee_equity_holders).
narrative_ontology:constraint_vindicates(valuation_legitimacy__real_options_technologist, real_options_pricing_theory).
narrative_ontology:constraint_vindicates(valuation_legitimacy__real_options_technologist, vertical_integration_compounding_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds 82.4% voting control on a 42% economic interest. Sets the price and timing of every tender offer, chooses which engineering milestones become public updates, and decides when the company raises money and at what mark. Personal credit lines and the financing of affiliated ventures are collateralized against this mark. Departure would mean abandoning the mission he describes as the point of his working life.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, musk_control_block, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Wrote checks at valuations two to three orders of magnitude below the current mark. Each upward repricing validates their original judgment and multiplies paper returns; several have sold small slices into secondaries at successive marks. They repeat the option-space argument to later buyers because it is the story of their own success.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, early_stage_investors, beneficiary,
    powerful, generational, mobile, global).

% Buy in periodic tender offers at a price the controlling holder alone announces, with limited information rights and no public filings to check the numbers against. Shares are illiquid between tenders; if option conversion stalls, they sit closest to the loss. Getting in at all has required relationships and allocation luck, and sitting out earlier rounds meant watching others gain.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, late_stage_tender_participants, payer,
    institutional, biographical, constrained, global).

% Take a large share of pay in stock subject to multi-year vesting. Rising marks make the packages life-changing on paper; the same concentration puts their savings and their careers in the same vehicle. Quitting forfeits unvested grants, so departure carries a double cost.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, employee_equity_holders, payer,
    moderate, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__real_options_technologist, employee_equity_holders, beneficiary).

% Operators of data centers and AI training fleets facing a projected 62 GW shortfall in U.S. power supply this decade. Their demand forecasts are the number that makes the orbital-compute segment's option value calculable. Today they sign no contracts with the company and set nothing; if orbiting compute ever ships, they are the customers who turn that option into revenue.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, ai_compute_buyers, beneficiary,
    organized, biographical, mobile, continental).

% The conditional beneficiary the reading itself declares: if a multiplanetary civilization ever exists, its founding surplus accrues to people who paid nothing for the option and were never asked. In the present tense this seat collects nothing, decides nothing, and cannot opt out; it is recorded to complete the declared beneficiary set and is not a present-day actor.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, humanity_option_holder, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(valuation_legitimacy__real_options_technologist, humanity_option_holder).

% Analysts and academics who price the same enterprise from booked revenue, EBITDA, and comparable transactions, and publish numbers far below the tender marks. No filing regime forces the company to answer them, tender windows open and close on the seller's schedule, and their work circulates without entering the room where the price is set.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, independent_dcf_valuation_community, excluded,
    institutional, biographical, analytical, global).

% Researchers in real-options finance and space economics who track whether option-priced ventures actually convert. They run conversion-rate studies, publish calibration audits, and take no position in the tenders; their work is the main outside check on whether the pricing language predicts anything.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, aerospace_finance_observers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__real_options_technologist, musk_control_block).
narrative_ontology:fixing_cost_class(valuation_legitimacy__real_options_technologist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared valuation language for ventures whose assets are unrealized capabilities: when cash-flow metrics return nothing, option-space pricing lets dispersed capital pool around long-horizon engineering programs, and lets a vertically integrated program be read as a correlated option portfolio rather than a conglomerate discount.
% TRANSFER_FUNCTION: Moves capital from investors - concentrated in late-stage tender participants buying at seller-set prices - into the company; moves legitimacy and pricing power to the controlling block; moves variance risk onto whoever buys nearest the top of the mark curve; promises a residual payoff to humanity-at-large only in the success branch.
% ABSENT_VOICES: The DCF valuation community and governance-form advocates are structurally absent: there is no public filing regime that compels engagement with their methods, and tender windows open and close on the seller's schedule. Present, they would argue the option premium is unfalsifiable as practiced and that the control structure, not the technology, sets the terms of every round.
% DISAPPEARANCE_RATIONALE: If the option-space legitimacy frame vanished overnight, the $1.75T mark would collapse toward cash-flow-supported value, tender participation would dry up, employee equity packages would lose their retention force, and capital would reallocate toward ventures with provable cash flows - the entire private-market pricing regime around the company would reorganize within quarters.
% FOUNDING_PROBLEM: How do you legitimately price a company whose assets are unrealized technological capabilities? Early on there were no comparable cash flows for reusable rockets or satellite broadband at scale; the option-space frame was built to make raising capital against hardware milestones defensible.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the academic real-options literature in financial economics independently identifies the pricing of pre-cash-flow deep-tech ventures as an open problem, and independent aerospace cost analyses treat milestone-based valuation as unresolved. The DCF community disputes the answer, not the existence of the problem. No corroboration exists for the specific ~6% probability weighting on the $28.5T portfolio TAM - that parameter is attested only inside the benefiting set.
narrative_ontology:disappearance_verdict(valuation_legitimacy__real_options_technologist, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__real_options_technologist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__real_options_technologist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(valuation_legitimacy__real_options_technologist, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__real_options_technologist, 0.34, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is moderate (0.34) because even this reading's own lights concede a narrative premium above fundamental option value plus a governance drag the option math does not price; it is not higher because the reading genuinely regards late entrants as informed buyers of convexity. Suppression (0.31) is informational rather than coercive: disclosure control, gated tender windows, and the social cost of invoking the rival framework inside the believer community. Theater_ratio (0.28) reflects mission rhetoric (Mars timelines, civilization-scale language) running ahead of the actual option mathematics, which is real and periodically updated. Accessibility_collapse is low (0.22): the DCF and governance frameworks remain fully available to anyone; nothing about the option frame forecloses them. Resistance is substantial (0.58): two sibling camps actively contest the frame's legitimacy claim. The measurement series run on one shared time grid (points 0-20, roughly 2005-2025); all three tracked metrics rise together because each upward repricing increased both the stakes of maintaining the frame and the enforcement effort (tender gating, disclosure curation) needed to maintain it - that enforcement-capacity intensification is exactly why suppression_requirement is tracked rather than left as a static scalar. End-state values match the base_properties scalars by construction of the grid.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently from the same structure. From the controlling block's position there is no constraint at all - it IS the enforcement mechanism, and the frame reads as the correct physics of valuation. From the late-stage participant's position the frame is the price of admission: the only language in which allocation happens, with the price announced unilaterally. Employees experience it as golden handcuffs denominated in the same units. Early investors experience vindication. The engine derives these divergent per-seat classifications from the structural data (role, power, exit); the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations map to real structural positions: the controlling block sits near the full-beneficiary end (sets the rules, collects the premium, cannot leave without dissolving the self); early investors sit low-d (entered cheap, mobile exits via secondaries); late-stage participants sit near the full-target end (pay the full mark, constrained exit, thin information); employees sit mid-high d with a dual declaration (pay concentration risk, receive upside). AI compute buyers are contingent beneficiaries - low d today, nothing collected yet. The humanity seat is declared for completeness of the reading's own beneficiary set but is a non-agent in present time and is excluded from directional arithmetic. Spatial scope is global with no audited public financials, which makes verification of option-value claims maximally difficult; the engine scales effective extraction upward accordingly. Suppression is authored as a raw structural property and is deliberately NOT scaled - only extractiveness rides directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   Calling this arrangement a snare would erase the genuine coordination achievement: a workable pricing language that funded reusable rockets and satellite broadband when cash-flow methods returned nothing. Calling it a rope would erase the unilateral price-setting, the control concentration, and the variance risk pushed onto the latest buyers. Tangled_rope preserves both halves: coordination function (shared option-pricing language) plus asymmetric extraction (seller-set marks accruing to one seat). The mandatrophy watch-point is internal to the frame's own success: Starlink's proven cash flows mean one of five segments no longer needs the option frame at all - it is now a DCF asset. If the remaining segments convert, the frame's necessity decays while its rhetorical machinery persists; the drift_state records this as minor practice_drift, and the founding_problem_status stays live only because four segments remain unproven. If orbital compute and Starship convert, expect pressure toward scaffold-like sunset of the option frame for the proven core - and expect the frame to resist that sunset, since the premium it licenses is largest precisely where proof is absent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the real_options_technologist reading of the valuation_legitimacy kernel; what structural deltas would the sibling readings (dcf_fundamentalist, musk_cult_believer, governance_skeptic) produce for the same standing arrangement?',
    'Compile the sibling stories as separate files and compare epsilon, beneficiary/victim sets, and computed types across the kernel; the disagreement is located in where legitimacy is grounded (cash-flow proof vs. priced optionality vs. founder epistemics vs. governance form).',
    'Under the dcf_fundamentalist reading the same arrangement computes far more extractive (four of five segments priced near zero); under governance_skeptic the victim set expands to all minority holders; under musk_cult_believer extraction drops toward zero and enforcement becomes purely reputational. Cross-reading comparison is the corpus-level measurement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame routing: one reading of a four-reading kernel; sibling structure lives in sibling files.').

omega_variable(
    option_probability_falsifiability,
    'Is the ~6% probability weight on the $28.5T portfolio TAM an empirically calibrated estimate or an unfalsifiable narrative parameter?',
    'Bayesian audit against the realized conversion rate of the frame''s prior option-class claims (reusability economics, launch cadence, Starlink margin ramp): did ex-ante probabilities assigned at earlier marks match ex-post outcomes?',
    'If systematically miscalibrated or unfalsifiable, the frame''s extraction exceeds its informational content and the arrangement slides toward snare-flavored operation; if calibrated, the coordination function dominates and the moderate epsilon stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(option_probability_falsifiability, empirical, 'Calibration status of the option-pricing model''s central probability parameter.').

omega_variable(
    compounding_vs_correlation,
    'Does vertical integration create compounding optionality (success in one segment raising joint success probability) or correlated failure exposure (one point of failure propagating across all segments)?',
    'Natural experiments already available: launch failures cascading into Starlink deployment windows; supplier and regulatory shocks hitting all segments simultaneously; engine anomalies stalling every downstream option at once.',
    'Genuine positive compounding supports the coordination half of the tangled_rope claim; correlated exposure converts the ''portfolio'' into one leveraged bet, raising effective extraction on late entrants who believed they held diversified optionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compounding_vs_correlation, empirical, 'Whether the integration premium reflects diversification or concentration.').

omega_variable(
    informed_risk_bearer_status,
    'Are late-stage tender participants genuinely informed risk-bearers (the low-victim-set assertion this reading makes) or structurally pressured buyers facing allocation rationing, FOMO dynamics, and thin information rights?',
    'Tender-participation data: oversubscription ratios, information rights actually granted, secondary-market exit spreads available between windows, and post-mark drawdown behavior of prior cohorts.',
    'If buyers are structurally pressured rather than informed, the victim set grows beyond the reading''s own declaration and effective extraction amplifies above the authored 0.34.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informed_risk_bearer_status, empirical, 'Whether the declared low victim set survives contact with tender mechanics.').

omega_variable(
    humanity_beneficiary_substance,
    'Is ''humanity benefits if multiplanetary civilization succeeds'' a substantive beneficiary position or a moralized cover that transfers the cost of optionality onto non-consenting parties (including taxpayers funding government launch contracts)?',
    'Trace contract cash flows and counterfactual public-alternative pricing for government missions; assess whether the civilizational-hedge payoff mechanism is specified anywhere outside promotional materials.',
    'If cover, the declared beneficiary set shrinks to capital holders and the coordination claim weakens correspondingly; the seat was authored as a non-agent precisely so it cannot inflate the coordination function arithmetically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(humanity_beneficiary_substance, preference, 'Substance versus cover-story status of the universal conditional beneficiary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__real_options_technologist, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vlopt_tech_tr_t0, valuation_legitimacy__real_options_technologist, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(vlopt_tech_tr_t0, observed).
narrative_ontology:measurement(vlopt_tech_tr_t4, valuation_legitimacy__real_options_technologist, theater_ratio, 4, 0.12).
narrative_ontology:measurement_basis(vlopt_tech_tr_t4, observed).
narrative_ontology:measurement(vlopt_tech_tr_t8, valuation_legitimacy__real_options_technologist, theater_ratio, 8, 0.17).
narrative_ontology:measurement_basis(vlopt_tech_tr_t8, observed).
narrative_ontology:measurement(vlopt_tech_tr_t12, valuation_legitimacy__real_options_technologist, theater_ratio, 12, 0.22).
narrative_ontology:measurement_basis(vlopt_tech_tr_t12, observed).
narrative_ontology:measurement(vlopt_tech_tr_t16, valuation_legitimacy__real_options_technologist, theater_ratio, 16, 0.25).
narrative_ontology:measurement_basis(vlopt_tech_tr_t16, observed).
narrative_ontology:measurement(vlopt_tech_tr_t20, valuation_legitimacy__real_options_technologist, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(vlopt_tech_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(vlopt_tech_be_t0, valuation_legitimacy__real_options_technologist, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(vlopt_tech_be_t0, observed).
narrative_ontology:measurement(vlopt_tech_be_t4, valuation_legitimacy__real_options_technologist, base_extractiveness, 4, 0.16).
narrative_ontology:measurement_basis(vlopt_tech_be_t4, observed).
narrative_ontology:measurement(vlopt_tech_be_t8, valuation_legitimacy__real_options_technologist, base_extractiveness, 8, 0.21).
narrative_ontology:measurement_basis(vlopt_tech_be_t8, observed).
narrative_ontology:measurement(vlopt_tech_be_t12, valuation_legitimacy__real_options_technologist, base_extractiveness, 12, 0.26).
narrative_ontology:measurement_basis(vlopt_tech_be_t12, observed).
narrative_ontology:measurement(vlopt_tech_be_t16, valuation_legitimacy__real_options_technologist, base_extractiveness, 16, 0.3).
narrative_ontology:measurement_basis(vlopt_tech_be_t16, observed).
narrative_ontology:measurement(vlopt_tech_be_t20, valuation_legitimacy__real_options_technologist, base_extractiveness, 20, 0.34).
narrative_ontology:measurement_basis(vlopt_tech_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(vlopt_tech_su_t0, valuation_legitimacy__real_options_technologist, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(vlopt_tech_su_t0, observed).
narrative_ontology:measurement(vlopt_tech_su_t4, valuation_legitimacy__real_options_technologist, suppression_requirement, 4, 0.14).
narrative_ontology:measurement_basis(vlopt_tech_su_t4, observed).
narrative_ontology:measurement(vlopt_tech_su_t8, valuation_legitimacy__real_options_technologist, suppression_requirement, 8, 0.19).
narrative_ontology:measurement_basis(vlopt_tech_su_t8, observed).
narrative_ontology:measurement(vlopt_tech_su_t12, valuation_legitimacy__real_options_technologist, suppression_requirement, 12, 0.24).
narrative_ontology:measurement_basis(vlopt_tech_su_t12, observed).
narrative_ontology:measurement(vlopt_tech_su_t16, valuation_legitimacy__real_options_technologist, suppression_requirement, 16, 0.28).
narrative_ontology:measurement_basis(vlopt_tech_su_t16, observed).
narrative_ontology:measurement(vlopt_tech_su_t20, valuation_legitimacy__real_options_technologist, suppression_requirement, 20, 0.31).
narrative_ontology:measurement_basis(vlopt_tech_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__real_options_technologist, information_standard).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__musk_cult_believer).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__governance_skeptic).

% DUAL FORMULATION NOTE:
% 'Valuation legitimacy' is a colloquial label covering at least four structurally distinct claims: legitimacy from discounted proven cash flows (dcf_fundamentalist), from the present value of technological option space (this file), from the founder's record of achieving impossible goals (musk_cult_believer), and from governance form protecting minority holders (governance_skeptic). Per the epsilon-invariance principle these are authored as separate stories sharing the kernel_id valuation_legitimacy and linked via affects_constraints; this story carries the option-space reading's own epsilon (moderate, self-assessed), which diverges sharply from what a dcf_fundamentalist file authors for the identical arrangement. The upstream/downstream structure runs both ways: the option frame borrows credibility from the DCF frame's proven segments (Starlink), while the DCF frame's hybrid models (decision-tree NPV) absorb pressure created by the option frame's market dominance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
