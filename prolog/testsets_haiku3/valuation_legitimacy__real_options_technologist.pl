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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Real Options Valuation Framework (Technologist Reading)
 *   domain: economic/technological/governance
 *
 * SUMMARY:
 *   SpaceX's $1.75T valuation is presented by technologists as legitimate
 *   option-value pricing: the firm holds a portfolio of technological options
 *   (Starlink proven; Starship developmental; orbital compute, lunar economy,
 *   Mars speculative) whose cumulative present value justifies the headline
 *   valuation under real-options frameworks. Vertical integration compounds
 *   optionality — success in one segment increases probability of success in
 *   others. This reading contrasts sharply with DCF-fundamentalist (which
 *   counts only Starlink's proven $7.2B EBITDA and treats speculation as
 *   noise), governance-skeptic (which sees Musk's voting control as
 *   extraction divorced from value creation), and cult-believer (which prices
 *   in Musk's track record as a leading indicator independent of financial
 *   models). The real-options technologist reading treats option-value as the
 *   legitimate, coherent frame for long-duration R&D capital. The constraint
 *   is the valuation legitimacy principle itself: what makes a $1.75T
 *   valuation coherent rather than speculative.
 *
 * KEY AGENTS:
 *   - Equity investors (organized, mobile exit): benefit from option-value pricing; can redeploy if thesis fails.
 *   - Debt holders (organized, constrained exit): provide capital but capture no upside; subordinate to equity tail outcomes.
 *   - Minority shareholders (moderate power, mobile exit): hold equity but have no governance voice; explicitly price option value via market participation.
 *   - Musk/founder control (institutional, identity-locked): sets portfolio composition and long-horizon capital discipline; from technologist seat, this is coordination mechanism, not extraction.
 *   - Alternative space operators (organized, constrained exit): excluded from capital-markets option-value consensus; face sequential disadvantage.
 *   - Regulatory authority (institutional, analytical): observes whether governance structure captures public-interest optionality or extracts private returns at public risk.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__real_options_technologist, 0.31).
domain_priors:suppression_score(valuation_legitimacy__real_options_technologist, 0.12).
domain_priors:theater_ratio(valuation_legitimacy__real_options_technologist, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, extractiveness, 0.31).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__real_options_technologist, rope).
narrative_ontology:human_readable(valuation_legitimacy__real_options_technologist, "Real Options Valuation Framework (Technologist Reading)").
narrative_ontology:topic_domain(valuation_legitimacy__real_options_technologist, "economic/technological/governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__real_options_technologist, 'c1d46339-806a-4e91-87de-7de60324b195').
narrative_ontology:cs_kernel_codification('c1d46339-806a-4e91-87de-7de60324b195', distributed).
narrative_ontology:cs_authority_grounding('c1d46339-806a-4e91-87de-7de60324b195', expertise).
narrative_ontology:cs_reading_relation('c1d46339-806a-4e91-87de-7de60324b195', valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('c1d46339-806a-4e91-87de-7de60324b195', valuation_legitimacy__musk_cult_believer, influences).
narrative_ontology:cs_reading_relation('c1d46339-806a-4e91-87de-7de60324b195', valuation_legitimacy__governance_skeptic, coexists_with).
narrative_ontology:cs_axiom('c1d46339-806a-4e91-87de-7de60324b195', foundational, real_options_value_is_present_value).
narrative_ontology:cs_axiom_status(real_options_value_is_present_value, holdable).
narrative_ontology:cs_axiom_grounding('c1d46339-806a-4e91-87de-7de60324b195', real_options_value_is_present_value, empirically_contingent).
narrative_ontology:cs_axiom('c1d46339-806a-4e91-87de-7de60324b195', foundational, vertical_integration_compounds_optionality).
narrative_ontology:cs_axiom_status(vertical_integration_compounds_optionality, holdable).
narrative_ontology:cs_axiom_grounding('c1d46339-806a-4e91-87de-7de60324b195', vertical_integration_compounds_optionality, empirically_contingent).
narrative_ontology:cs_reference_frame('c1d46339-806a-4e91-87de-7de60324b195', academic_finance_real_options_framework).
narrative_ontology:cs_drift_state('c1d46339-806a-4e91-87de-7de60324b195', contemporary_space_capital_markets_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c1d46339-806a-4e91-87de-7de60324b195', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__real_options_technologist, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, early_option_holders).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, technological_civilization).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, equity_investors).
narrative_ontology:constraint_victim(valuation_legitimacy__real_options_technologist, debt_holders).
narrative_ontology:constraint_victim(valuation_legitimacy__real_options_technologist, minority_shareholders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold equity stakes priced at expected value of real options portfolio. Gain from vertical integration enabling cross-segment probability compounding (success in Starlink increases Starship viability; Starship success unlocks orbital compute and lunar economy optionality). Their exit is liquid market-based; they can redeploy capital if the reading proves wrong. Information asymmetry exists (private financials) but not suppression — investors explicitly price option value and acknowledge tail-risk concentration.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, equity_investors, beneficiary,
    organized, generational, mobile, global).

% Provide capital at rates reflecting underlying technology risk but cannot participate in upside. If core technology fails (Starship development delays, Starlink market saturation), debt becomes subordinate to equity claims on residual assets. Their constraint is structural: debt pricing at fixed rates while options holders capture all tail optionality.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, debt_holders, payer,
    organized, biographical, constrained, global).

% Hold equity but have no governance voice (Musk controls 82.4% of voting rights with 42% equity ownership). Under the real-options reading, they benefit from optionality pricing if it is accurate; under governance-skeptic reading, they bear extraction. From the real-options perspective, their stake is their explicit signal they accept the option-value thesis; their exit is liquid public markets.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, minority_shareholders, payer,
    moderate, biographical, mobile, global).

% The vertically integrated architecture creates positive externalities: Starlink subsidizes Starship development, orbital infrastructure reduces launch costs globally, data from both programs accelerates AI/ML for autonomous systems. This is not a collected agent but a vindicated proposition: that closed-loop technological integration solves coordination problems open markets cannot address as fast.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, technology_ecosystem, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(valuation_legitimacy__real_options_technologist, technology_ecosystem).

% Sets the option portfolio composition, capital allocation across segments, and governance structure that locks founder vision to long time horizons. The real-options reading treats this lock-in as necessary: dispersed governance would pressure for Starlink cash extraction rather than Starship capital intensity. From this seat, the reading is that founder control IS the coordination mechanism that makes real-options valuation coherent.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, musk_as_founder_control, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Blue Origin, Axiom, others can build subsystems but face sequential entry disadvantage as SpaceX vertical integration speeds learning loops. They would contest that the real-options reading privileges monopoly-corridor development over competitive option exploration; they are excluded from the capital markets' option-pricing consensus.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, alternative_space_operators, excluded,
    organized, generational, constrained, global).

% The FCC (spectrum allocation), FAA (launch licensing), and Congress (national space policy) observe whether the real-options reading generates public-interest outcomes or whether Musk's control extracts private optionality at public risk. From this seat, the reading is contestable: real options might justify high valuation, but governance structure determines if public benefits are captured.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, regulatory_authority, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% FOUNDING_PROBLEM: Long-duration, high-risk R&D projects (20–50 year timescales) require capital sources aligned with their payoff horizons. Debt markets demand repayment in 5–10 years; traditional public equity markets reward cash flows within 5 years; venture capital exits in 7–10 years. A firm pursuing 50-year goals cannot reliably fund development through markets that demand intermediate returns. SpaceX's founding problem was: How do you build rockets to Mars when financial markets do not price 50-year optionality?
% FOUNDING_PROBLEM_CORROBORATION: Academic finance literature (Dixit & Pindyck 1994, Pindyck 2007) establishes that real-options frameworks are theoretically sound for valuing long-duration R&D. Venture capital industry practice (Sequoia, Benchmark, Accel pricing early-stage tech firms on option value, not cash flow) attests the problem is live. Competing space operators (Blue Origin's New Shepard, Axiom Space's stations) copying SpaceX's vertical integration and founder control structure attests the problem is live. NASA's difficulty in long-horizon projects (SLS 40-year development, cost overruns) attests the problem from the public-sector side. Non-benefiting parties: academic critics of option-value pricing (who argue it enables speculation bubbles), governance-reform advocates, and minority shareholders all attest the problem is live by contesting the framework rather than denying the time-horizon mismatch.
narrative_ontology:founding_problem_status(valuation_legitimacy__real_options_technologist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__real_options_technologist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(valuation_legitimacy__real_options_technologist, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__real_options_technologist, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is authored as LOW (0.31 at interval end) because the real-options reading treats the valuation as legitimate coordination, not extraction: investors explicitly accept option-value pricing, debt holders price their subordination, and the arrangement persists through market consensus rather than suppression. Suppression is LOW (0.12) because there is no coercion holding the constraint — exit costs are market-based (opportunity cost of capital elsewhere) not structural or legal. Theater ratio is LOW (0.18) because the real-options framework is operationally coherent: it drives capital allocation decisions (Starship funding, orbital compute R&D, Starlink growth) that are materially consequential; the financial narrative is not performative cover for a different operation. Accessibility collapse is moderate (0.42) because alternatives exist: rival space operators can pursue similar strategies (Blue Origin has tried), public space agencies (NASA) can fund long-duration research through budget processes, and private investors can fund speculative tech through venture/growth equity. The constraint is not natural law — it is a coherent but contestable reading. Resistance is HIGH (0.58) because multiple parties actively reject the real-options reading: governance-skeptics argue it masks extraction; DCF fundamentalists argue it inflates valuations; alternative operators argue it privileges monopoly corridors. This resistance is not suppressed; it appears in SEC filings, activist campaigns, congressional testimony, and competing capital allocation frameworks. The measurement series tracks extractiveness rising slightly (as Musk's control solidifies post-acquisition behavior) and theater_ratio rising slowly (as spectacle around SpaceX goals increases but core operational discipline persists).
 *
 * PERSPECTIVAL GAP:
 *   The largest gap is between the real-options technologist seat (valuation is legitimate coordination across long time horizons) and the governance-skeptic seat (valuation is cover for extraction by founder control). This is not a measurement gap — it is a fundamentally different reading of the same kernel (valuation legitimacy). The technologist reading maps Musk's control to the beneficiary side (coordination mechanism); governance-skeptic maps it to the agenda-setter-extracter side (enables concentrated gains). The engine computes per-seat types; the technologist seat should compute ROPE or weak TANGLED (coordination with some asymmetry but no suppression), while the skeptic seat should compute SNARE or strong TANGLED. This divergence IS the signal that the constraint contains genuine contestation: different readings are not observational variations on one truth; they are incommensurable frames with different beneficiary/victim assignments.
 *
 * DIRECTIONALITY LOGIC:
 *   The real-options technologist reading produces asymmetric directionality: equity investors benefit from the framework (low d, subsidy-side) because option-value pricing gives them higher expected returns than DCF alternatives; debt holders pay through subordination (high d, extraction-side); minority shareholders sit near symmetric (their explicit market participation signals they price the option value themselves); founder control extracts authority but from the technologist perspective provides the coordination mechanism that makes option-value coherent (moderate d, beneficiary-side in this reading, would be target-side in governance-skeptic reading). The reading is internally consistent: different seats experience the same constraint differently because they occupy different structural positions relative to option upside capture. The directionality is NOT overridden because the beneficiary/victim declarations are precise: equity investors genuinely benefit; debt holders genuinely pay; minority shareholders explicitly opt in.
 *
 * MANDATROPHY ANALYSIS:
 *   The real-options reading does NOT exhibit mandatrophy. The founding problem (long-duration R&D needs a capital structure matching its time horizon) is LIVE: all evidence confirms it persists. The constraint (valuation legitimacy via real options) continues to solve that problem. Where mandatrophy would appear: if SpaceX shifted to cash-extraction mode (dividend payments, margin focus, reduced capital intensity), the real-options justification would become zombie — the founding problem would still exist but the constraint would no longer address it. Such a shift would reclassify the constraint to PITON (theater rising, extractiveness climbing without coordination function). The current state shows increasing theater but steady extractiveness and continuous capital intensity, which is consistent with the coordination reading persisting.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    option_portfolio_thesis_empirical_test,
    'Does vertical integration in SpaceX actually compound optionality as the real-options framework claims, or is success in Starlink independent of Starship/orbital outcomes?',
    'Historical analysis of capital allocation decisions, engineering interdependencies between programs, and correlation of technical progress across segments. If Starlink success materially increased Starship timeline acceleration or reduced failure probability, thesis is supported; if programs advanced independently, thesis is weakened.',
    'If compounding optionality is confirmed empirically, the real-options valuation is more defensible and extractiveness should be revised downward (more pure coordination, less rent); if independence is confirmed, the valuation appears more speculative and extractiveness should rise (less legitimate coordination claim).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(option_portfolio_thesis_empirical_test, empirical, 'Empirical validation of compounding optionality thesis.').

omega_variable(
    governance_extraction_vs_coordination,
    'Does founder voting control (82.4% of votes with 42% equity) operate as a coordination mechanism for long-horizon R&D, or as an extraction mechanism enabling private gains from public risk?',
    'Comparative institutional analysis: Do governance-constrained firms (SpaceX if voting control were equalized) achieve equivalent long-horizon R&D progress? Does Musk''s control enable capital discipline that would vanish with dispersed voting? Does the control extract private value (personal travel, status) beyond what valuation requires?',
    'If coordination: the governance structure is rent-justified and extractiveness remains low (Musk''s control is the price of coherent option-value pricing). If extraction: extractiveness rises to 0.45-0.55 range; the reading reclassifies toward TANGLED_ROPE (coordination with significant asymmetry); the governance-skeptic reading gains empirical support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(governance_extraction_vs_coordination, conceptual, 'Whether founder control is coordination mechanism or extraction vehicle.').

omega_variable(
    tail_risk_distribution_opacity,
    'Is the distribution of tail outcomes (Mars success, orbital compute TAM realization, lunar economy dominance) knowable from public information, or is it necessarily opaque because it depends on Musk''s undisclosed strategic decisions?',
    'Track Musk''s capital allocation decisions against public statements; assess whether announced plans (Mars, orbital compute, Starlink growth rates) are subsequently changed; compare pre-announcement option values with post-announcement outcomes.',
    'If distribution is knowable from public signals, the real-options pricing can be contested rationally (DCF vs. option value = different discount rates on same risk). If distribution is opaque (depends on Musk''s private judgments), the valuation is priced on trust/reputation rather than financial framework; this shifts the reading toward cult-believer dynamics and raises suppression (minority shareholders cannot contest decisions they cannot predict).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tail_risk_distribution_opacity, empirical, 'Opacity of tail outcome distribution as source of valuation uncertainty.').

omega_variable(
    kernel_reading_contest,
    'Which reading of the valuation-legitimacy kernel is actually operative in capital markets: real-options technologist, DCF fundamentalist, governance-skeptic, or Musk cult-believer?',
    'Forensic analysis of actual SpaceX equity transaction data: What prices do equity trades clear at? Do minority investors execute trades at valuations consistent with option-value models (pricing compounding optionality) or DCF fundamentals (valuing only Starlink cash flow) or reputation priors (Musk-track-record premium)? Does volatility pricing in equity options reflect real-options framework or fundamental uncertainty?',
    'If market behavior is consistent with real-options pricing, the reading is validated empirically and extractiveness is low (the market consensus is rational). If market behavior shows scattered signals (some trades DCF-like, others option-like, some reputation-like), the constraint is not a single coherent reading but a contested mixture, and the assigned extractiveness of 0.31 (pure real-options reading) is wrong; multiple constraint stories are needed (one per reading, with different extractiveness values per stakeholder seat).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Which kernel reading governs actual capital market behavior for SpaceX equity.').

omega_variable(
    sibling_reading_foreclosure_contingency,
    'If the real-options thesis is empirically confirmed (compounding optionality is real, tail outcomes are knowable), do the sibling readings (DCF fundamentalist, governance-skeptic, cult-believer) remain logically holdable, or does confirmation foreclose them?',
    'Logical analysis: Can a DCF fundamentalist rationally reject option-value pricing even if compounding optionality is confirmed? Can a governance-skeptic rationally maintain that founder control is extraction even if governance discipline demonstrably increases option realization? Can a cult-believer rationally maintain that track record is primary signal even if financial models predict outcomes as well?',
    'If sibling readings remain holdable despite confirmation (different epistemologies, different values), then reading_relations are COEXISTS_WITH (current). If confirmation forecloses siblings (their core premises become indefensible), then relations shift to FORECLOSES — a much rarer and structurally significant transition. This determines whether the kernel resolves to a single victor or remains perpetually contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_contingency, conceptual, 'Whether empirical confirmation of real-options thesis forecloses sibling readings or leaves them holdable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__real_options_technologist, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t0, valuation_legitimacy__real_options_technologist, theater_ratio, 0, 0.08).
narrative_ontology:measurement(valu_tr_t5, valuation_legitimacy__real_options_technologist, theater_ratio, 5, 0.11).
narrative_ontology:measurement(valu_tr_t10, valuation_legitimacy__real_options_technologist, theater_ratio, 10, 0.14).
narrative_ontology:measurement(valu_tr_t15, valuation_legitimacy__real_options_technologist, theater_ratio, 15, 0.16).
narrative_ontology:measurement(valu_tr_t20, valuation_legitimacy__real_options_technologist, theater_ratio, 20, 0.17).
narrative_ontology:measurement(valu_tr_t25, valuation_legitimacy__real_options_technologist, theater_ratio, 25, 0.18).
narrative_ontology:measurement(valu_tr_t30, valuation_legitimacy__real_options_technologist, theater_ratio, 30, 0.18).

% Extraction over time
narrative_ontology:measurement(valu_be_t0, valuation_legitimacy__real_options_technologist, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(valu_be_t5, valuation_legitimacy__real_options_technologist, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(valu_be_t10, valuation_legitimacy__real_options_technologist, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(valu_be_t15, valuation_legitimacy__real_options_technologist, base_extractiveness, 15, 0.28).
narrative_ontology:measurement(valu_be_t20, valuation_legitimacy__real_options_technologist, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(valu_be_t25, valuation_legitimacy__real_options_technologist, base_extractiveness, 25, 0.31).
narrative_ontology:measurement(valu_be_t30, valuation_legitimacy__real_options_technologist, base_extractiveness, 30, 0.31).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(valuation_legitimacy__real_options_technologist, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__real_options_technologist, resource_allocation).
narrative_ontology:boltzmann_floor_override(valuation_legitimacy__real_options_technologist, 0.18).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__musk_cult_believer).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__governance_skeptic).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the contested kernel VALUATION_LEGITIMACY. The kernel is the question: what makes a $1.75T SpaceX valuation coherent? Each reading instantiates a different constraint with a different epsilon (extractiveness), beneficiary/victim structure, and type classification. The real-options-technologist reading (this story) claims extractiveness is LOW (0.31) because the valuation is legitimate coordination; the governance-skeptic reading should show extractiveness RISING (toward 0.65-0.75) because the same facts are interpreted as extraction; the DCF-fundamentalist reading should show the valuation as INFLATED rather than extractive (different axis — is the number right, not is it fair). All four are linked via network.affects_constraints; they share the same interval and many of the same stakeholders but with different role assignments (Musk's control is coordination in technologist, extraction in skeptic). The constraint family exists because a single natural-language concept (SpaceX valuation) decomposes into multiple structurally distinct claims under different epistemological frames.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
