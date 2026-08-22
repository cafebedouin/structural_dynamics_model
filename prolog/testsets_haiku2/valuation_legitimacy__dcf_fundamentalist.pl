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
 *   human_readable: DCF Fundamentalist Valuation Legitimacy (Proven Cash Flows Only)
 *   domain: corporate_finance/technology_governance/space_economics
 *
 * SUMMARY:
 *   The DCF Fundamentalist reading treats valuation legitimacy as strictly
 *   dependent on discounting proven, contractually secured cash flows. Under
 *   this reading, SpaceX's $1.75T valuation (at $18.7B revenue and -$4.9B net
 *   loss, yielding a 93x revenue multiple) is fundamentally unjustifiable.
 *   Only Starlink's $4.4B operating profit—contractually secured through
 *   government procurement—supports a $44–88B valuation (10–20x earnings).
 *   Orbital AI, Mars colonization, and full-stack vertical integration are
 *   treated as speculative R&D: economically valuable as options on future
 *   technologies, but not to be capitalized into equity value until they
 *   produce proven cash flows. The reading identifies a high-extraction
 *   structure: early venture investors and insiders benefit by exiting at the
 *   inflated peak; public equity investors absorb the downside when DCF
 *   discipline reasserts.
 *
 * KEY AGENTS:
 *   - DCF fundamentalist community: enforces proven-cash-flow discipline through research, capital allocation, and institutional practice
 *   - Public equity investors: bear the arbitrage loss; enter at inflated valuations, exit after repricing
 *   - Early venture investors: capture the spread by exiting before DCF reasserts; arbitrage beneficiaries
 *   - Insider liquidators (Musk et al.): use secondary markets to convert inflated valuation into liquidity before constraint enforcement
 *   - Starlink's proven cash flows: the ~$44-88B justified valuation subset under pure DCF
 *   - Unproven R&D portfolio (orbital AI, Mars): treated as options, excluded from asset capitalization
 *   - Real-options technologists: excluded by the reading's core axiom; argue optionality itself has present value
 *   - Cult-believer retail investors: excluded by refusal to accept cash-flow metrics as legitimate
 *   - Governance reformers: excluded by the reading's focus on cash flows rather than voting-control asymmetry
 *   - SEC/regulatory observers: decide whether to permit opaque aggregation of proven and unproven R&D in disclosed valuations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__dcf_fundamentalist, 0.81).
domain_priors:suppression_score(valuation_legitimacy__dcf_fundamentalist, 0.67).
domain_priors:theater_ratio(valuation_legitimacy__dcf_fundamentalist, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, extractiveness, 0.81).
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(valuation_legitimacy__dcf_fundamentalist, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__dcf_fundamentalist, tangled_rope).
narrative_ontology:human_readable(valuation_legitimacy__dcf_fundamentalist, "DCF Fundamentalist Valuation Legitimacy (Proven Cash Flows Only)").
narrative_ontology:topic_domain(valuation_legitimacy__dcf_fundamentalist, "corporate_finance/technology_governance/space_economics").

domain_priors:requires_active_enforcement(valuation_legitimacy__dcf_fundamentalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__dcf_fundamentalist, '48a33ea5-6ff3-4ec6-a57b-92c846be39a2').
narrative_ontology:cs_kernel_codification('48a33ea5-6ff3-4ec6-a57b-92c846be39a2', distributed).
narrative_ontology:cs_authority_grounding('48a33ea5-6ff3-4ec6-a57b-92c846be39a2', extraction).
narrative_ontology:cs_interpretation_layer_present('48a33ea5-6ff3-4ec6-a57b-92c846be39a2').
narrative_ontology:cs_reading_relation('48a33ea5-6ff3-4ec6-a57b-92c846be39a2', valuation_legitimacy__real_options_technologist, influences).
narrative_ontology:cs_reading_relation('48a33ea5-6ff3-4ec6-a57b-92c846be39a2', valuation_legitimacy__musk_cult_believer, forecloses).
narrative_ontology:cs_reading_relation('48a33ea5-6ff3-4ec6-a57b-92c846be39a2', valuation_legitimacy__governance_skeptic, coexists_with).
narrative_ontology:cs_axiom('48a33ea5-6ff3-4ec6-a57b-92c846be39a2', foundational, legitimacy_requires_proven_cash_flows).
narrative_ontology:cs_axiom_status(legitimacy_requires_proven_cash_flows, holdable).
narrative_ontology:cs_axiom_grounding('48a33ea5-6ff3-4ec6-a57b-92c846be39a2', legitimacy_requires_proven_cash_flows, empirically_contingent).
narrative_ontology:cs_axiom('48a33ea5-6ff3-4ec6-a57b-92c846be39a2', secondary, unproven_rd_zero_asset_value).
narrative_ontology:cs_axiom_status(unproven_rd_zero_asset_value, holdable).
narrative_ontology:cs_axiom_grounding('48a33ea5-6ff3-4ec6-a57b-92c846be39a2', unproven_rd_zero_asset_value, instrumental).
narrative_ontology:cs_reference_frame('48a33ea5-6ff3-4ec6-a57b-92c846be39a2', cash_flow_primacy).
narrative_ontology:cs_drift_state('48a33ea5-6ff3-4ec6-a57b-92c846be39a2', contemporary_2024, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('48a33ea5-6ff3-4ec6-a57b-92c846be39a2', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__dcf_fundamentalist, early_venture_investors).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__dcf_fundamentalist, insider_liquidators).
narrative_ontology:constraint_victim(valuation_legitimacy__dcf_fundamentalist, public_equity_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Professional investors, equity analysts, and financial institutions that enforce discounted-cash-flow valuation discipline. They produce research reports, credit assessments, and capital allocation decisions. They argue that valuation derived from unproven R&D is theater unless the R&D produces contractually secured future cash flows. Their enforcement is social (reputational cost for non-compliance with DCF discipline) and institutional (withholding capital, downgrading, litigation support).
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, dcf_fundamentalist_community, agenda_setter,
    institutional, biographical, mobile, global).

% Retail and institutional investors who buy equity at valuations divorced from proven cash flows. At $1.75T valuation against $18.7B revenue and -$4.9B net loss, they pay a 93x revenue multiple. Their exit is constrained because the constraint's enforcement (marketing, analyst herding, FOMO narratives) makes staying cheaper than the certainty of missing an upside. They bear the risk when proved cash flows fail to materialize.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, public_equity_investors, payer,
    organized, biographical, constrained, global).

% Early-stage capital providers (seed, Series A-D) who bought in at $100M–$10B valuations. The DCF fundamentalist constraint permits them to exit at the speculative peak without cashing out the underlying cash flow to justify their returns. They capture the spread between their entry valuation and the inflated public valuation before the constraint's eventual enforcement.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, early_venture_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Company insiders (founder, early employees, board member) with access to secondary markets and pre-IPO liquidation paths. The constraint's enforcement delay permits them to sell equity at inflated valuations before DCF discipline reasserts. Musk's liquidation of $42B in equity at multiples disconnected from SpaceX's $4.4B Starlink profit exemplifies the seat.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, insider_liquidators, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__dcf_fundamentalist, insider_liquidators, agenda_setter).

% The proven-cash-flow component (Starlink operations: $4.4B operating profit, contractually secured government revenue). This subset justifies a $44-88B valuation (10-20x earnings multiple). The DCF fundamentalist reading isolates this subset and applies DCF discipline to it alone, treating orbital AI and Mars colonization as speculative R&D.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, starlink_cashflow_subset, beneficiary,
    powerful, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(valuation_legitimacy__dcf_fundamentalist, starlink_cashflow_subset).

% Orbital AI, Mars colonization, full-stack vertical integration, autonomous booster recovery. These R&D programs are technologically novel but lack proven contractual cash flows. The DCF fundamentalist reading excludes them from asset valuation and treats them as options (economically valuable, but not to be capitalized as equity value until cash-flow proof arrives).
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, unproven_rd_portfolio, excluded,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(valuation_legitimacy__dcf_fundamentalist, unproven_rd_portfolio).

% Finance theorists and practitioners who value optionality itself: the present value of future-possible outcomes from R&D. They dispute that unproven tech should be zero-valued; instead, they argue that vertical integration creates compounding optionality worth $500B–$1T. They are excluded from the DCF fundamentalist conversation by its core axiom (proven flows only).
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, real_options_technologist_community, excluded,
    institutional, biographical, constrained, global).

% Institutional investors, proxy advisors, and regulators focused on minority shareholder protection. They contest whether 82.4% voting control with 42% equity stake is a valuation legitimacy issue or a governance legitimacy issue. Their exclusion from the fundamentalist framing means their voting-control analysis never enters the valuation equation.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, governance_reform_advocates, excluded,
    organized, biographical, constrained, national).

% Retail investors and media figures whose investment thesis rests on founder track record and vision statements rather than cash-flow metrics. They dismiss DCF analysis as 'boring' and interpret it as a failure of discipline imagination. They are excluded by the fundamentalist reading's axiom that legitimacy requires proven cash flows, not founder narrative.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, cult_believer_community, excluded,
    organized, biographical, identity_locked, global).

% Securities regulators and accountants responsible for disclosure and valuation standards. They observe whether companies are required to separately state Starlink's proven cash flows and unproven R&D valuations in financial statements. The constraint's enforcement rests partly on their choice of whether to permit opaque aggregation.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__dcf_fundamentalist, sec_sec_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__dcf_fundamentalist, insider_liquidators).
narrative_ontology:fixing_cost_class(valuation_legitimacy__dcf_fundamentalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, auditable standard for what constitutes a legitimate valuation: discounting contractually proven cash flows at a risk-adjusted rate. This solves the coordination problem of comparing company value across sectors (proven infrastructure vs. speculative technology) and across time horizons (cash available now vs. cash hoped-for from unproven R&D). Without this standard, every investor applies their own narrative framework, and capital markets become story contests rather than price discovery mechanisms.
% TRANSFER_FUNCTION: Moves the spread between intrinsic valuation (43–88B, using DCF on Starlink) and market valuation ($1.75T) from patient early investors and insiders to public market entrants. Early VCs and insiders capture arbitrage by exiting at the inflated peak; public investors absorb the downside when the constraint's enforcement returns (valuations repricing toward proven cash flows).
% ABSENT_VOICES: Real-options technologists are excluded by definitional choice (unproven tech is not valued); founder-cult believers are excluded by their refusal to engage cash-flow metrics; governance reformers who see voting-control asymmetry as the actual constraint are excluded because the fundamentalist framing treats governance as orthogonal to valuation.
% DISAPPEARANCE_RATIONALE: If the DCF fundamentalist constraint vanished—if markets accepted unproven R&D as equally valid to proven cash flows for valuation—public-market pricing would immediately reallocate to narrative and founder-track-record models. Insider exit strategies would lose arbitrage windows. Early investors could no longer time their liquidation to the speculative peak. The repricing would cascade through linked markets (credit, equity, M&A) as the constraint's enforcement machinery dissolved.
% FOUNDING_PROBLEM: Early venture and private-equity capital needed a disciplined method to distinguish genuine value creation (verifiable cash flows) from narrative inflation (founder genius, speculative upside). DCF provides that discipline: it forces the question 'what cash will this actually generate, and when?' Without it, every speculative pitch looks identical to a proven business.
% FOUNDING_PROBLEM_CORROBORATION: Academic finance (Damodaran, Zubrow), regulatory practice (SEC valuation guidance), and institutional investor policy (CalPERS, BlackRock engagement criteria) all attest the problem is still live. Audit firms cite valuation disputes as their highest-risk assertion. Independent analysis from equity research shops that do not carry insider relationships (short sellers, independent analysts) attests the constraint's enforcement is actively resisted by market actors who benefit from narrative inflation.
narrative_ontology:disappearance_verdict(valuation_legitimacy__dcf_fundamentalist, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__dcf_fundamentalist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__dcf_fundamentalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(valuation_legitimacy__dcf_fundamentalist, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__dcf_fundamentalist, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.65 at interval start to 0.81 by midpoint, then plateaus—reflecting the constraint's growing enforcement as insider liquidation reaches its peak and public investors accumulate. Suppression starts at 0.51 and rises to 0.67: the constraint requires continuous work to suppress competing narratives (founder-track-record, optionality stories, cult-believer retail activity) that would justify the inflated valuation if permitted into capital-allocation decisions. Theater ratio climbs from 0.38 to 0.58 as the gap between cash-flow reality and valuation narrative widens: marketing, analyst cheerleading, and founder vision statements increasingly dominate over cash-flow discussion. Accessibility collapse at 0.72 reflects that once an investor understands DCF discipline, the illusion of the inflated valuation partly collapses—but practical barriers to exit (low liquidity windows, tax consequences, belief persistence) keep many trapped. Resistance at 0.44 indicates moderate pushback from cult-believers and options-theorists, but the institutional structure (credit markets, regulatory scrutiny, audit firms) biases toward DCF enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the fundamentalist seat, the constraint is legitimate coordination: it forces clarity about which parts of the business are actually cash-generative. From the early-investor and insider seats, the constraint is a temporary arbitrage window with a known expiration date (when DCF reasserts). From the public-investor seat, the constraint is extraction: they are trapped in a narrative game they thought was a cash-flow-based valuation. From the real-options seat, the constraint is a misframing: it treats unproven R&D as zero-value rather than as economically valuable optionality. From the cult-believer seat, the constraint is a category error: it treats founder track record as irrelevant when it is the strongest signal available. The engine computes these divergences from the structural data—beneficiary/victim declarations and exit constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Early venture investors and insider liquidators are the primary beneficiaries: they benefit from the constraint's delay in enforcement (the 24-month window it provides before DCF discipline fully reasserts) and the spread between their entry valuations and the public market peak. Public equity investors are the primary targets: they absorb the valuation risk asymmetrically. The DCF fundamentalist community (analysts, risk managers, institutional investors) is the agenda-setter: they produce and enforce the discipline through research, capital denial, litigation support, and credit market repricing. The real-options technologist and cult-believer communities sit near the excluded end: their directionality is constrained (they cannot arbitrage the constraint as insiders can) and their exit is identity-locked (many believers cannot fathom a framework that discounts founder vision). Starlink itself, as a cash-flow subset, benefits from the constraint's partitioning (it receives 10–20x earnings valuation instead of being diluted in aggregate metrics), but is subject to DCF enforcement—not extractive from its perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (distinguish genuine value creation from narrative inflation) remains live—academic finance, audit practice, and institutional investors all attest to it. However, the constraint's enforcement has degraded over the 24-month interval: the rise in theater_ratio (0.38 → 0.58) and suppression_requirement (0.51 → 0.67) indicates that the fundamentalist discipline is losing traction. Insider liquidation is complete before enforcement cycles; cult-believer retail activity is suppressing short-selling and analyst criticism; and real-options narratives are being incorporated into institutional frameworks, partly neutralizing the DCF-only reading. The constraint is not yet mandatrophic (the founding problem is still live, enforcement still occurs), but the trajectory suggests a future shift where the DCF fundamentalist reading competes equally with real-options and narrative frameworks rather than dominating. The classification remains tangled_rope (coordination function + asymmetric extraction) rather than snare (pure extraction with no coordination) because the constraint does solve a real coordination problem—it provides an auditable standard for value comparison across companies and sectors—even as it creates extraction via insider arbitrage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unproven_rd_valuation_boundary,
    'Where exactly is the boundary between speculative R&D (zero-valued as assets, valued only as options) and early-stage revenue programs (counted as proven cash flows)? Orbital AI has development-stage revenue; Mars missions have no revenue yet. Does Orbital AI cross the threshold to asset status?',
    'Regulatory guidance (SEC ruling on R&D capitalization vs. expensing) or accounting standards (GAAP/IFRS clarification). Also natural experiment: if Orbital AI secures $10B+ in government contracts, it becomes proven-cash-flow and the boundary shifts.',
    'A narrow boundary (only operational, profitable segments count) strengthens the fundamentalist reading and increases the unjustified-valuation verdict. A wider boundary (any contractually committed development revenue counts) weakens the verdict and aligns closer to real-options framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unproven_rd_valuation_boundary, empirical, 'Boundary definition: when does speculative R&D become proven cash flow?').

omega_variable(
    insider_exit_arbitrage_window,
    'Is the 24-month window during which insiders can liquidate at inflated valuations before DCF enforcement cycles a permanent feature of the market, or a temporary asymmetry that will close as regulatory scrutiny tightens?',
    'Observed data: track insider liquidation rates and public-market repricing cycles across comparable companies over 5+ years. Also regulatory change: if SEC mandates separate disclosure of proven vs. unproven valuation components, the window compresses to weeks.',
    'If the window is permanent, insider extraction is structurally locked and the constraint remains a tangled rope with high ongoing extraction. If the window closes, the constraint transitions toward pure coordination (no extraction path), or pure snare (if suppression increases to trap public investors).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(insider_exit_arbitrage_window, empirical, 'Permanence of the insider arbitrage window').

omega_variable(
    cult_believer_suppression_mechanism,
    'Is the suppression of fundamentalist DCF narrative (theater_ratio 0.38 → 0.58, suppression_requirement 0.51 → 0.67) driven by structural barriers (short-selling restrictions, retail-investor access to leverage) or by internalized identity fusion (retail believers cannot emotionally accept DCF analysis because it threatens their founder-worship framework)?',
    'Post-exit trajectory: if public investors who sustain losses can shift their narrative away from founder-worship and accept DCF analysis, suppression is structural; if they defend the narrative even after experiencing the repricing loss, suppression is substantially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the 0.67 metric suggests—cult-believers carry the suppression with them after exit. If structural, regulatory remedies (short-selling access, transparency mandates) could weaken suppression without changing the cult-believer identity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cult_believer_suppression_mechanism, conceptual, 'Structural vs. internalized suppression of DCF narrative').

omega_variable(
    kernel_reading_contest_asymmetry,
    'This reading (DCF fundamentalist) competes against three sibling readings (governance, cult, real-options) in one kernel (valuation legitimacy). Are these readings truly at parity in institutional power, or does the DCF fundamentalist reading hold institutional dominance that makes the contest asymmetric?',
    'Institutional audit: track the prevalence of each reading across asset managers'' proxy voting records, credit rating agencies'' methodologies, regulatory guidance, and audit firms'' valuation standards over 10 years. Also epistemic audit: count academic papers and professional certifications (CFA, CFP) that teach each reading.',
    'If DCF fundamentalist is institutionally dominant, the contest is not a symmetric coexistence but a dominant-narrative-with-suppressed-alternatives structure, which would reclassify the constraint from tangled_rope to snare. If parity, the tangled_rope reading is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_asymmetry, empirical, 'Institutional parity or asymmetry among kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__dcf_fundamentalist, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t0, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 0, 0.38).
narrative_ontology:measurement(valu_tr_t3, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 3, 0.43).
narrative_ontology:measurement(valu_tr_t6, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 6, 0.48).
narrative_ontology:measurement(valu_tr_t9, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 9, 0.52).
narrative_ontology:measurement(valu_tr_t12, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 12, 0.55).
narrative_ontology:measurement(valu_tr_t15, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 15, 0.57).
narrative_ontology:measurement(valu_tr_t18, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 18, 0.58).
narrative_ontology:measurement(valu_tr_t24, valuation_legitimacy__dcf_fundamentalist, theater_ratio, 24, 0.58).

% Extraction over time
narrative_ontology:measurement(valu_be_t0, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(valu_be_t3, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 3, 0.7).
narrative_ontology:measurement(valu_be_t6, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 6, 0.74).
narrative_ontology:measurement(valu_be_t9, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 9, 0.77).
narrative_ontology:measurement(valu_be_t12, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 12, 0.79).
narrative_ontology:measurement(valu_be_t15, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 15, 0.8).
narrative_ontology:measurement(valu_be_t18, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 18, 0.81).
narrative_ontology:measurement(valu_be_t24, valuation_legitimacy__dcf_fundamentalist, base_extractiveness, 24, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(valu_su_t0, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 0, 0.51).
narrative_ontology:measurement(valu_su_t3, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 3, 0.55).
narrative_ontology:measurement(valu_su_t6, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 6, 0.59).
narrative_ontology:measurement(valu_su_t9, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 9, 0.62).
narrative_ontology:measurement(valu_su_t12, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 12, 0.64).
narrative_ontology:measurement(valu_su_t15, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(valu_su_t18, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 18, 0.67).
narrative_ontology:measurement(valu_su_t24, valuation_legitimacy__dcf_fundamentalist, suppression_requirement, 24, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__dcf_fundamentalist, resource_allocation).
narrative_ontology:boltzmann_floor_override(valuation_legitimacy__dcf_fundamentalist, 0.12).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy__governance_skeptic).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy__real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__dcf_fundamentalist, valuation_legitimacy__musk_cult_believer).

% DUAL FORMULATION NOTE:
% This constraint is one reading (DCF fundamentalist) of the contested kernel VALUATION_LEGITIMACY. All four readings (dcf_fundamentalist, governance_skeptic, real_options_technologist, musk_cult_believer) share the same referent—SpaceX's $1.75T public valuation at $18.7B revenue and -$4.9B net loss—but instantiate different constraints because they ask different legitimacy questions and identify different beneficiary/victim structures. The DCF reading isolates proven cash flows and treats unproven R&D as zero-valued assets; governance_skeptic isolates voting-control asymmetry and treats that as the legitimacy problem; real_options_technologist isolates optionality value and argues unproven tech should be capitalized; cult_believer isolates founder track record and treats cash flows as lagging indicators. Each reading produces its own ε (0.81, 0.76, 0.34, 0.12 respectively—ordered by extraction magnitude). The network edges track how each reading's enforcement affects the others: DCF fundamentalist suppresses real-options narrative, influences governance skeptic (adds urgency), forecloses pure cult-believer math.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(valuation_legitimacy__dcf_fundamentalist, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
