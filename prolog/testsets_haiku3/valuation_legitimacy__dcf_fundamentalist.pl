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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: valuation_legitimacy__dcf_fundamentalist
 *   human_readable: DCF Fundamentalist Reading: Valuation Legitimacy via Proven Cash Flows
 *   domain: corporate_finance/technology_governance/space_economics
 *
 * SUMMARY:
 *   This constraint instantiates the DCF fundamentalist reading of a
 *   contested valuation legitimacy kernel. The reading asserts that
 *   legitimate equity valuation derives from discounting proven cash flows;
 *   speculative, unproven technologies (orbital AI, Mars colonization) are
 *   research investments and options, not revenue-generating assets, and
 *   should not be capitalized in equity valuation. At $18.7B revenue, $4.9B
 *   annual net loss, and $1.75T equity valuation, this gives a 93x revenue
 *   multiple and negative earnings multiple—fundamentally unjustifiable under
 *   DCF axioms. Starlink's $4.4B annual operating profit supports a $44-88B
 *   valuation (10-20x earnings). The high valuation is maintained by
 *   suppressing cash-flow-based scrutiny and elevating speculative narrative
 *   (Musk's past execution, optionality, Mars colonization upside) to the
 *   legitimacy domain. Public equity buyers and institutional minorities pay
 *   the extraction (overvalued equity); Musk's control coalition and early
 *   investor exit windows benefit. The constraint is the narrative
 *   enforcement that prevents DCF fundamentalism from becoming the binding
 *   standard—not a natural fact (it is a chosen epistemic frame) but an
 *   actively maintained suppressant on a competing frame (real-options,
 *   technologist, governance-remedial readings).
 *
 * KEY AGENTS:
 *   - musk_control_coalition (institutional power, 82.4% voting control, agenda-setter): enforces speculative narrative, suppresses DCF scrutiny, maintains high valuation to liquidate control premium
 *   - public_equity_buyers (powerless at point of purchase, mobile exit): buy at peak valuations under suppressed DCF understanding; face realized losses if valuation corrects
 *   - early_investor_exit_window (powerful institutional players, arbitrage exit): benefit from peak-valuation liquidation; exit window closes when valuation corrects
 *   - institutional_minority_holders (organized, constrained exit): carry fiduciary duty but structurally locked out of governance remedies; face dilution if high-valuation narrative persists in secondary offerings
 *   - equity_analysts (moderate power, constrained by client/reputational risk): caught between DCF-using clients and equity-holding portfolio managers; publish muted criticisms
 *   - alternate_valuation_tradition (non-agent, analytical seat): real-options, technologist, and governance-skeptic readings are logically incompatible with DCF fundamentalism; their exclusion from mainstream analysis is enforced by control-coalition narrative dominance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__dcf_fundamentalist, 0.87).
domain_priors:suppression_score(valuation_legitimacy__dcf_fundamentalist, 0.72).
domain_priors:theater_ratio(valuation_legitimacy__dcf_fundamentalist, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, extractiveness, 0.87).
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__dcf_fundamentalist, snare).
narrative_ontology:human_readable(valuation_legitimacy__dcf_fundamentalist, "DCF Fundamentalist Reading: Valuation Legitimacy via Proven Cash Flows").
narrative_ontology:topic_domain(valuation_legitimacy__dcf_fundamentalist, "corporate_finance/technology_governance/space_economics").

domain_priors:requires_active_enforcement(valuation_legitimacy__dcf_fundamentalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__dcf_fundamentalist, 'c8c93d79-1860-4781-9c4e-63340ebcd126').
narrative_ontology:cs_kernel_codification('c8c93d79-1860-4781-9c4e-63340ebcd126', distributed).
narrative_ontology:cs_authority_grounding('c8c93d79-1860-4781-9c4e-63340ebcd126', extraction).
narrative_ontology:cs_reading_relation('c8c93d79-1860-4781-9c4e-63340ebcd126', valuation_legitimacy__real_options_technologist, forecloses).
narrative_ontology:cs_reading_relation('c8c93d79-1860-4781-9c4e-63340ebcd126', valuation_legitimacy__musk_cult_believer, forecloses).
narrative_ontology:cs_reading_relation('c8c93d79-1860-4781-9c4e-63340ebcd126', valuation_legitimacy__governance_skeptic, coexists_with).
narrative_ontology:cs_axiom('c8c93d79-1860-4781-9c4e-63340ebcd126', foundational, cash_flow_primacy_in_valuation).
narrative_ontology:cs_axiom_status(cash_flow_primacy_in_valuation, holdable).
narrative_ontology:cs_axiom_grounding('c8c93d79-1860-4781-9c4e-63340ebcd126', cash_flow_primacy_in_valuation, empirically_contingent).
narrative_ontology:cs_axiom('c8c93d79-1860-4781-9c4e-63340ebcd126', foundational, speculative_assets_are_options_not_equity).
narrative_ontology:cs_axiom_status(speculative_assets_are_options_not_equity, holdable).
narrative_ontology:cs_axiom_grounding('c8c93d79-1860-4781-9c4e-63340ebcd126', speculative_assets_are_options_not_equity, empirically_contingent).
narrative_ontology:cs_reference_frame('c8c93d79-1860-4781-9c4e-63340ebcd126', standard_dcf_valuation_framework).
narrative_ontology:cs_drift_state('c8c93d79-1860-4781-9c4e-63340ebcd126', musk_control_suppression_era_2023_2025, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c8c93d79-1860-4781-9c4e-63340ebcd126', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__dcf_fundamentalist, musk_control_coalition).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__dcf_fundamentalist, early_investor_exit_window).
narrative_ontology:constraint_victim(valuation_legitimacy__dcf_fundamentalist, public_equity_buyers).
narrative_ontology:constraint_victim(valuation_legitimacy__dcf_fundamentalist, institutional_minority_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Musk holds 82.4% voting control with 42% equity stake. Sets valuation narrative, controls board discourse, and liquidates control premium to early investors and through secondary offerings. Benefits from suppressing cash-flow-based scrutiny; uses speculative technologist framing to justify $1.75T valuation against $18.7B revenue and $4.9B annual net loss. Exit is available through staged liquidation; costs are borne by remaining shareholders.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, musk_control_coalition, agenda_setter,
    institutional, biographical, arbitrage, global).

% Retail and institutional passive-index holders buy at peak valuations ($1.75T) under the narrative that speculative R&D (orbital AI, Mars colonization) justifies 93x revenue multiple with negative earnings. The DCF fundamentalist reading makes this a known overvaluation; most buyers lack analytic capacity to do independent valuation. Exit cost is opportunity cost and realized loss; exit timing is constrained by liquidity windows.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, public_equity_buyers, payer,
    powerless, biographical, mobile, global).

% Venture and seed-stage investors who backed Musk's ventures at earlier valuations ($5-50B range) benefit from the current $1.75T peak to execute liquidation at 35-350x returns. Their exit depends on maintaining the speculative narrative long enough to hold the valuation at or near peak. Once the window closes (either through market correction or governance pressure), their ability to exit at peak diminishes.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, early_investor_exit_window, beneficiary,
    powerful, biographical, arbitrage, global).

% Pension funds, endowments, and asset managers holding minority stakes. Locked into valuations set by the control coalition and cannot influence board decisions (Musk's voting control forecloses minority governance remedies). Carry fiduciary duty to beneficiaries but are structurally excluded from audit rights and director nomination—the constraint is the voting arrangement itself, which this reading shows as extractive.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, institutional_minority_holders, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__dcf_fundamentalist, institutional_minority_holders, excluded).

% Sell-side analysts and rating agencies face reputational risk from rating the equity as overvalued (triggering equity-holding client backlash and trading-desk complaints) versus maintaining credibility with DCF-using institutional clients. Most publish 'hold' or 'reduce' ratings while downplaying magnitude of the valuation disconnect; some publish strong sells and face professional isolation.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, equity_analysts, observer,
    moderate, biographical, constrained, global).

% The real-options and technologist framings that justify high valuation on optionality and Musk's execution track record are logically incompatible with DCF fundamentalism's core claim. They are not beneficiaries or payers but the epistemic competitors to this reading; their exclusion from mainstream equity analyst discourse is enforced by the control coalition's narrative authority.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, alternate_valuation_tradition, excluded,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(valuation_legitimacy__dcf_fundamentalist, alternate_valuation_tradition).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__dcf_fundamentalist, musk_control_coalition).
narrative_ontology:fixing_cost_class(valuation_legitimacy__dcf_fundamentalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Financial accounting standards and equity valuation norms exist to coordinate investor expectations and capital allocation around a shared measurement basis (discounted cash flows). DCF fundamentalism operationalizes this coordination by insisting on matching valuation to demonstrated, auditable cash generation.
% TRANSFER_FUNCTION: Transfers $1.75T in market capitalization premium from public equity buyers to early investors and the control coalition, relative to what DCF-justified valuations ($44-88B for verified cash-flow-generating operations plus speculative R&D option value) would support. The transfer mechanism is suppression of cash-flow-based scrutiny and elevation of speculative narrative as legitimate valuation input.
% ABSENT_VOICES: Employees with equity compensation (paper millionaires at peak valuation); debt holders and creditors (low-risk, unaffected by equity valuation); competing space/satellite operators (excluded by narrative dominance); minority shareholders who would vote for governance remedies if their votes mattered. SEC enforcement personnel with mandate to scrutinize disclosure adequacy but constrained by safe-harbor rules for forward-looking statements.
% DISAPPEARANCE_RATIONALE: If the DCF fundamentalist reading were to become the binding narrative (through regulatory enforcement, market correction, or governance displacement), the valuation would compress from $1.75T to $44-88B range within months. Early investors would face liquidation deadlines and depressed exit prices. Public shareholders would experience write-downs. The control coalition's negotiating position for secondary offerings would deteriorate. Capital allocation into speculative R&D would contract unless funded differently (government contracts, debt, slower equity raise). The constraint that suppresses DCF discipline is actively enforcing the high valuation; its removal would reorganize equity capital flows and Musk's liquidity strategy.
% FOUNDING_PROBLEM: How should investors value companies with unproven, capital-intensive technology paths (space launch, Mars colonization, orbital AI) where cash generation is negative and decades away? Early justification: the Starlink constellation (proven commercial operation, $4.4B operating profit) validates the competency; the R&D spending is leverage on proven ability.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem—whether a technologist with demonstrated execution capability in one domain (space launch via Starlink) can credibly pursue speculative R&D in adjacent domains—was resolved circa 2023 when Starlink achieved sustained profitability. The proof exists. The DCF fundamentalist reading asserts that this resolution means speculative R&D (orbital AI, Mars colonization) should be valued as options, not assets, with limited impact on core valuation. Equity analysts at major firms (Morgan Stanley, Wedbush), SEC enforcement actions on disclosure, and academic finance literature on option-adjusted valuation all corroborate this shift. The control coalition continues to suppress the implication (low valuation) while keeping the proof (Starlink's execution).
narrative_ontology:disappearance_verdict(valuation_legitimacy__dcf_fundamentalist, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__dcf_fundamentalist, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__dcf_fundamentalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(valuation_legitimacy__dcf_fundamentalist, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__dcf_fundamentalist, 0.87, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.87) because the constraint redistributes $1.6-1.7T in market capitalization from public shareholders to early investors and the control coalition, relative to DCF-justified valuations ($44-88B + option premium). This is not a coordination gain split asymmetrically; it is a pure transfer enabled by suppression of the competing analytical frame. Suppression is high (0.72) because the constraint's persistence depends actively on preventing cash-flow-based scrutiny from becoming standard practice—every secondary offering, every earnings call, every institutional holding decision involves suppression of DCF implications. Theater is moderate-high (0.58) because much of the enforcement activity takes the form of narrative performance (Musk's visionary framing, future-option storytelling, Mars-colony messaging) rather than structural change (Starlink operation is real, but its $4.4B profit is real and doesn't justify a $1.75T total valuation). Accessibility collapse is moderate (0.61): alternative frames (real-options, governance skepticism) exist and are partially accessible to institutional analysts, but are suppressed by control-coalition narrative authority and cultural faith in Musk. Resistance is substantial (0.68): some equity analysts publish strong sells; SEC enforcement has examined disclosure; academic finance critiques the valuation. The measurement series show extraction rising over 30 months (0.71→0.87) as secondary offerings increase and early investor exit windows crystallize valuations; theater rising (0.38→0.58) as enforcement shifts from fundamental performance to narrative dominance; suppression rising (0.60→0.72) as the control coalition hardens defensive narrative positions against increasing DCF skepticism.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (musk_control_coalition) experiences the constraint as coordination of investor expectations around a legitimate future-value frame. Their narrative: 'Starlink proves execution competency; orbital AI and Mars are optionality; the $1.75T valuation is rational because it prices in the option value of those programs at scale.' Their exit is available (secondary offerings, collateral liquidation); they control the narrative authority. The payer seats (public_equity_buyers, institutional_minorities) experience the constraint as suppressed cash-flow analysis and forced-choice between overvalued equity and exclusion from the market. Their frame: 'We can see Starlink generates $4.4B, which justifies maybe $50-80B; orbital AI is 10+ years from revenue; the $1.75T is 20-35x too high, and we're locked in.' The divergence arises from the control coalition's ability to suppress DCF methodology from mainstream analysis; if DCF became standard, valuations would compress within weeks and early-investor exit would be unavailable. The constraint is the institutional enforcement of the high-valuation narrative, not a natural fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Musk's control coalition has d near 1.0 (full target of beneficiary extraction)—their structural position is to extract and maintain the extraction mechanism. Early investors have d near 0.9 (full beneficiary, facing exit window deadline); their interest in maintaining the high valuation is existential. Public equity buyers have d near 0.8 (high target): they bear the extraction and have mobile exit (can liquidate at any time, but at prevailing—and rising—prices that still embed the premium). Institutional minorities have d near 0.85 (high target, trapped): their exit is structurally constrained by fiduciary-duty holding periods and index-tracking requirements. Equity analysts have d near 0.5 (symmetric, constrained): they see the truth (DCF implies ~$44-88B) but face equal costs from both overvaluation (client embarrassment if they downrate too early) and undervaluation (portfolio-manager backlash if they downrate correctly). The divergence is radical: from Musk's analytical position, the $1.75T valuation is justified by optionality and his execution track record (d near 0, beneficiary frame); from public shareholders' position, it is unjustifiable overvaluation (d near 0.8, target frame). The engine computes this divergence from the structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—how to value companies pursuing unproven, capital-intensive R&D with negative near-term cash flows—was legitimately unsolved in 2015-2018. Starlink's achievement of sustained $4.4B operating profit in 2024-2025 resolves the founding problem: we have proof that the technologist can execute in capital-intensive domains. The DCF fundamentalist reading argues that resolution should produce a revaluation downward (from speculative $1.75T to $44-88B core plus modest option premium). The control coalition's response is to suppress this implication while keeping the proof (Starlink's profit). The mandatrophy is the dead founding problem persisting as active constraint: the arrangement persists not because the founding problem is live, but because it benefits the agenda-setter and early investors to maintain the suppression. No party expects the founding problem to return (Musk's execution is proven); all parties expect the valuation to compress if DCF becomes standard. The constraint is a snare, not a tangled rope or scaffold, because the coordination function (valuing innovation risk) is decoupled from the extraction mechanism (suppressing cash-flow scrutiny), and the constraint persists only because suppression is actively enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which valuation legitimacy framework is epistemically sound: discounted cash flows (DCF fundamentalist), real-options pricing (technologist), track-record extrapolation (Musk-cult), or governance-remedial (skeptic)?',
    'Market mechanism: if a competitor achieves sustained higher profitability with lower speculative R&D spending, DCF wins empirically; if Musk delivers Mars-scale civilization and orbital AI, technologist reading retroactively validated; if governance intervention compresses valuation and improves shareholder returns, skeptic governance thesis vindicated.',
    'The entire constraint classification pivots on which reading becomes the binding narrative. DCF fundamentalist reading classifies as snare (asymmetric extraction). Real-options reading reclassifies as tangled rope (genuine optionality coordination + extraction). Technologist reading reclassifies as rope (coordination around innovation risk). Skeptic reading targets governance overhaul, not valuation methodologies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel reading contest: four incompatible valuation legitimacy framings, each internally coherent, each claiming epistemic authority. Which becomes the binding standard?').

omega_variable(
    cash_flow_unprovability,
    'Are orbital AI and Mars colonization legitimately unproven cash flows, or are they currently unprovable (by construction impossible to model with confidence intervals)?',
    'Proof by attempted valuation: if DCF models can be constructed for orbital AI revenue (e.g., data processing for Earth observation, comsat competition) with defensible assumptions, the assets move into option-value territory. If no credible revenue model can be constructed (reputational/scientific goals only), they remain R&D, not assets. Expert testimony from aerospace finance specialists outside the beneficiary coalition.',
    'If unprovable, the DCF fundamentalist reading argues valuation should cap at Starlink''s justified value ($44-88B) plus a small R&D premium (5-10%). If merely unproven but modellable, option pricing moves into effect and allows higher valuations. The difference is whether uncertainty is empirical (reducible through evidence) or structural (irreducible by definition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cash_flow_unprovability, empirical, 'Whether speculative cash flows can be modeled with confidence intervals (option pricing territory) or are structurally unmeasurable (R&D expense category).').

omega_variable(
    musk_personal_extraction,
    'How much of the valuation premium serves Musk''s personal liquidation strategy (secondary offerings, collateral for other ventures, executive compensation calibration) versus genuine value-creation incentives?',
    'Disclosure of Musk''s documented secondary offerings, collateral borrowing against equity stakes, and equity compensation decisions; comparison of Musk''s realized liquidation proceeds to historical peer executives in comparable valuations; expert forensics on equity compensation design (whether designed to align with public shareholder returns or personal liquidity milestones).',
    'If personal liquidation is the primary driver of the high valuation being maintained, the constraint is a pure snare (extraction mechanism). If valuation correlates with strategic R&D spending (e.g., Mars program milestones), the extraction is mixed with genuine strategic leverage and the constraint is tangled rope. The answer reshapes whether Musk is an agenda-setter in a snare or a strategic beneficiary in a mixed arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(musk_personal_extraction, empirical, 'Degree to which the high valuation is motivated by Musk''s personal extraction versus strategic capital allocation.').

omega_variable(
    reading_sibling_foreclosure,
    'Does the DCF fundamentalist reading logically foreclose the real-options and technologist readings, or do they coexist as different methodological frames for the same underlying uncertainty?',
    'Formal analysis: DCF fundamentalism asserts that only cash flows are assets; real-options asserts that uncertainty itself has value independent of modeled cash flows. These are incompatible truth-claims if both claim exhaustive valuation authority. If DCF argues for lower valuations and technologist argues for higher, they cannot both be right in the same framework. Test whether any framework can coherently hold both axioms simultaneously.',
    'If DCF forecloses technologist (true incompatibility), this reading''s snare classification stands and the technologist reading''s tangled-rope classification is incoherent (the engine would flag a contradiction). If they coexist as methodological choices, both readings remain live and the engine computes per-seat divergence (agenda-setter uses technologist, payer uses DCF).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_sibling_foreclosure, conceptual, 'Whether DCF and real-options valuations are logically incompatible or merely different methodological frames on the same uncertainty.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the suppression of DCF scrutiny structural (control coalition actively prevents analysis) or internalized (public investors self-censor because faith in Musk is culturally dominant)?',
    'Post-narrative-shift trajectory: if DCF fundamentalist reading became widely accepted and analysis became common, would suppression lift (structural) or would public investor confidence remain high (internalized)? Natural experiment from markets where different narratives dominate (e.g., European investors more skeptical than U.S. retail).',
    'If structural, the constraint persists as long as the control coalition enforces suppression; removal of control would lift suppression. If internalized, suppression persists even after the control coalition loses media dominance; public shareholders carry the bias forward. Internalized suppression indicates higher effective extraction and stronger snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether suppression of cash-flow-based scrutiny is actively enforced or culturally internalized in investor behavior.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__dcf_fundamentalist, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t0, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(valu_tr_t0, observed).
narrative_ontology:measurement(valu_tr_t6, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 6, 0.44).
narrative_ontology:measurement_basis(valu_tr_t6, observed).
narrative_ontology:measurement(valu_tr_t12, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 12, 0.5).
narrative_ontology:measurement_basis(valu_tr_t12, observed).
narrative_ontology:measurement(valu_tr_t18, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 18, 0.54).
narrative_ontology:measurement_basis(valu_tr_t18, observed).
narrative_ontology:measurement(valu_tr_t24, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 24, 0.57).
narrative_ontology:measurement_basis(valu_tr_t24, observed).
narrative_ontology:measurement(valu_tr_t30, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 30, 0.58).
narrative_ontology:measurement_basis(valu_tr_t30, observed).
narrative_ontology:measurement(valu_tr_t36, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 36, 0.59).
narrative_ontology:measurement_basis(valu_tr_t36, projected).

% Extraction over time
narrative_ontology:measurement(valu_be_t0, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 0, 0.71).
narrative_ontology:measurement_basis(valu_be_t0, observed).
narrative_ontology:measurement(valu_be_t6, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 6, 0.75).
narrative_ontology:measurement_basis(valu_be_t6, observed).
narrative_ontology:measurement(valu_be_t12, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 12, 0.81).
narrative_ontology:measurement_basis(valu_be_t12, observed).
narrative_ontology:measurement(valu_be_t18, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 18, 0.84).
narrative_ontology:measurement_basis(valu_be_t18, observed).
narrative_ontology:measurement(valu_be_t24, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 24, 0.86).
narrative_ontology:measurement_basis(valu_be_t24, observed).
narrative_ontology:measurement(valu_be_t30, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 30, 0.87).
narrative_ontology:measurement_basis(valu_be_t30, observed).
narrative_ontology:measurement(valu_be_t36, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 36, 0.87).
narrative_ontology:measurement_basis(valu_be_t36, projected).

% Suppression requirement over time
narrative_ontology:measurement(valu_su_t0, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 0, 0.6).
narrative_ontology:measurement_basis(valu_su_t0, observed).
narrative_ontology:measurement(valu_su_t6, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 6, 0.64).
narrative_ontology:measurement_basis(valu_su_t6, observed).
narrative_ontology:measurement(valu_su_t12, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 12, 0.68).
narrative_ontology:measurement_basis(valu_su_t12, observed).
narrative_ontology:measurement(valu_su_t18, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 18, 0.7).
narrative_ontology:measurement_basis(valu_su_t18, observed).
narrative_ontology:measurement(valu_su_t24, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 24, 0.71).
narrative_ontology:measurement_basis(valu_su_t24, observed).
narrative_ontology:measurement(valu_su_t30, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(valu_su_t30, observed).
narrative_ontology:measurement(valu_su_t36, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 36, 0.73).
narrative_ontology:measurement_basis(valu_su_t36, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__dcf_fundamentalist, resource_allocation).
narrative_ontology:boltzmann_floor_override(valuation_legitimacy__dcf_fundamentalist, 0.12).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy__real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy__musk_cult_believer).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy__governance_skeptic).

% DUAL FORMULATION NOTE:
% This constraint is part of a four-member constraint family decomposing the contested 'valuation_legitimacy' kernel. Each family member instantiates a different reading: DCF fundamentalist (this story), real-options technologist, Musk-cult believer, governance skeptic. The four readings are structurally incompatible—they cannot all be true simultaneously in a single epistemic framework. The family is linked because each reading's classification and metrics depend partly on understanding what the other readings claim and where the empirical/conceptual disagreement is located. The DCF fundamentalist reading classifies as snare (pure extraction, suppressed methodology); the real-options reading classifies as tangled rope (genuine optionality + extraction); the technologist reading may classify as rope or tangled rope depending on whether vertical integration delivers credible synergies; the governance skeptic reading is orthogonal to DCF and focuses on governance structures, not valuation methodology. Each family member contributes evidence to understanding which reading is epistemically sound: market outcomes (if equity prices collapse when analysts shift to DCF, that falsifies real-options high-valuation claims), regulatory action (if SEC mandates DCF-style cash-flow reconciliation, that institutionalizes DCF), and Musk's execution (if orbital AI or Mars colonization achieve revenue milestones, that retroactively validates the technologist reading). The network edge reflects dependency: the DCF reading's classification as snare depends partly on whether the real-options and technologist readings can credibly model the speculative cash flows (if they can, valuation is option-pricing territory; if they cannot, the high valuation is pure extraction with no option-value anchor).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(valuation_legitimacy__dcf_fundamentalist, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
