% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__musk_cult_believer
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: valuation_legitimacy__musk_cult_believer
 *   human_readable: Founder Track-Record Valuation Legitimacy (Believer Reading)
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The standing arrangement under contest: a valuation regime in which
 *   Musk-affiliated ventures are legitimated by the founder's record of
 *   delivering goals the industry called impossible, while financial metrics
 *   are reframed as lagging indicators that systematically undershoot
 *   frontier value. The believer community supplies the marginal capital and
 *   the enforcement — moderating doubt, squeezing shorts, outvoting
 *   governance reform — that holds the regime in place. KEY AGENTS (by
 *   structural relationship): musk_control_block: agenda-setter and principal
 *   recipient (institutional/arbitrage) — sets the narrative, administers the
 *   packages it justifies, receives the capital it raises;
 *   buy_and_hold_believers: primary beneficiary seat
 *   (organized/identity_locked) — funds and defends the regime, bears its
 *   drawdowns; short_sellers: primary target (powerful/constrained) — bears
 *   squeeze losses; institutional_governance_investors: secondary target
 *   (organized/mobile) — ballots nullified, exit by divestment;
 *   independent_analysts_and_skeptics: excluded voice (moderate/mobile) —
 *   present in the market, purged from the community; passive_index_holders:
 *   diffuse bearer (moderate/constrained); musk_affiliated_ventures:
 *   beneficiary entity (institutional/arbitrage); securities_regulators:
 *   analytical observer (institutional/analytical). Family note: this file is
 *   ONE reading of the valuation_legitimacy kernel; the dcf_fundamentalist,
 *   real_options_technologist, and governance_skeptic readings are separate
 *   constraints with their own epsilon and victim sets, linked via
 *   network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__musk_cult_believer, 0.4).
domain_priors:suppression_score(valuation_legitimacy__musk_cult_believer, 0.62).
domain_priors:theater_ratio(valuation_legitimacy__musk_cult_believer, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, extractiveness, 0.4).
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__musk_cult_believer, tangled_rope).
narrative_ontology:human_readable(valuation_legitimacy__musk_cult_believer, "Founder Track-Record Valuation Legitimacy (Believer Reading)").
narrative_ontology:topic_domain(valuation_legitimacy__musk_cult_believer, "economic/technological").

domain_priors:requires_active_enforcement(valuation_legitimacy__musk_cult_believer).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__musk_cult_believer, '09d0d83a-76af-46cc-aacf-28176ea1c1c0').
narrative_ontology:cs_kernel_codification('09d0d83a-76af-46cc-aacf-28176ea1c1c0', distributed).
narrative_ontology:cs_authority_grounding('09d0d83a-76af-46cc-aacf-28176ea1c1c0', practice).
narrative_ontology:cs_interpretation_layer_present('09d0d83a-76af-46cc-aacf-28176ea1c1c0').
narrative_ontology:cs_reading_relation('09d0d83a-76af-46cc-aacf-28176ea1c1c0', valuation_legitimacy__dcf_fundamentalist, forecloses).
narrative_ontology:cs_reading_relation('09d0d83a-76af-46cc-aacf-28176ea1c1c0', valuation_legitimacy__real_options_technologist, coexists_with).
narrative_ontology:cs_reading_relation('09d0d83a-76af-46cc-aacf-28176ea1c1c0', valuation_legitimacy__governance_skeptic, forecloses).
narrative_ontology:cs_axiom('09d0d83a-76af-46cc-aacf-28176ea1c1c0', foundational, founder_track_record_grounds_valuation).
narrative_ontology:cs_axiom_status(founder_track_record_grounds_valuation, holdable).
narrative_ontology:cs_axiom_grounding('09d0d83a-76af-46cc-aacf-28176ea1c1c0', founder_track_record_grounds_valuation, empirically_contingent).
narrative_ontology:cs_axiom('09d0d83a-76af-46cc-aacf-28176ea1c1c0', secondary, capability_trumps_governance_structures).
narrative_ontology:cs_axiom_status(capability_trumps_governance_structures, holdable).
narrative_ontology:cs_axiom_grounding('09d0d83a-76af-46cc-aacf-28176ea1c1c0', capability_trumps_governance_structures, empirically_contingent).
narrative_ontology:cs_reference_frame('09d0d83a-76af-46cc-aacf-28176ea1c1c0', execution_track_record_primacy).
narrative_ontology:cs_drift_state('09d0d83a-76af-46cc-aacf-28176ea1c1c0', contemporary_post_trillion_dollar_package_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('09d0d83a-76af-46cc-aacf-28176ea1c1c0', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__musk_cult_believer, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__musk_cult_believer, musk_control_block).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__musk_cult_believer, buy_and_hold_believers).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__musk_cult_believer, musk_affiliated_ventures).
narrative_ontology:constraint_victim(valuation_legitimacy__musk_cult_believer, short_sellers).
narrative_ontology:constraint_victim(valuation_legitimacy__musk_cult_believer, independent_analysts_and_skeptics).
narrative_ontology:constraint_victim(valuation_legitimacy__musk_cult_believer, institutional_governance_investors).
narrative_ontology:constraint_victim(valuation_legitimacy__musk_cult_believer, passive_index_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(valuation_legitimacy__musk_cult_believer, buy_and_hold_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls board composition and voting outcomes at the affiliated companies through supermajority voting rights held with a minority of economic equity. Sets the public narrative — production targets, product timelines, colony commitments — and administers executive compensation packages whose milestones are defined by that same narrative. Raises capital and sells shares into the valuation the narrative supports, and can redirect personal attention and capital across ventures if any single venture's story sours.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, musk_control_block, agenda_setter,
    institutional, generational, arbitrage, global).

% Retail and community investors who coordinate through social platforms, forums, and fan media; they supply the marginal buying that sustains valuations through quarters when financial statements show losses. Early entrants hold large paper gains; later entrants carry heavier exposure at higher average cost. Selling is socially coded as betrayal, doubt is removed from community spaces as disruption, and membership carries identity rewards — being part of a historic transition to multiplanetary life. Exit means realizing losses, leaving the community, and revising a self-concept built around the mission.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, buy_and_hold_believers, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__musk_cult_believer, buy_and_hold_believers, payer).

% Institutional traders who borrow and sell shares expecting statement-based repricing. Their research is publicly dismissed as manipulation, their positions are periodically squeezed by coordinated retail buying and momentum flows, and closing requires repurchasing shares at prices set by the very narrative they dispute. Several funds have absorbed outsized losses; pressing the thesis further exposes them to renewed squeezes and reputational cost.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, short_sellers, payer,
    powerful, biographical, constrained, global).

% Asset managers and proxy advisors who file proposals on board independence, compensation size, and dual-class structures. Their ballots are arithmetically outweighed by the founder's voting block regardless of how other holders split, so proposals fail predictably. Many respond by trimming or exiting positions, which remains available to them at market prices; their remaining influence runs through engagement letters and public statements rather than votes.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, institutional_governance_investors, payer,
    organized, biographical, mobile, global).

% Sell-side and independent analysts, journalists, and academics who publish statement-based valuations and risk assessments. Their work circulates in the broad market but is removed from the believer community's coordinating spaces, where it is labeled as hostile noise; publishing bearish work has drawn harassment campaigns directed at the analysts personally. Being wrong is career-damaging, and being right before the crowd shifts earns no reward from the community being critiqued.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, independent_analysts_and_skeptics, excluded,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__musk_cult_believer, independent_analysts_and_skeptics, payer).

% Pension funds, retirement accounts, and index-fund holders who own the shares because indexes include them. They absorb dilution from large compensation grants and live with governance decisions made by the voting block, with no consent channel beyond proxy votes they cannot win. Exiting means departing the index or accepting tracking error; most simply hold.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, passive_index_holders, payer,
    moderate, biographical, constrained, global).

% The operating companies raise capital, compensate employees, and pursue acquisitions at valuations set by the track-record narrative rather than by current cash flow. Capital access without covenant-style metric discipline funds long-horizon programs — booster reuse, constellation buildout, autonomy — that balance-sheet-constrained competitors cannot attempt. Their fortunes rise and fall with a narrative they did not independently choose.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, musk_affiliated_ventures, beneficiary,
    institutional, generational, arbitrage, global).

% Agencies that review disclosures, executive compensation plans, and market conduct around the affiliated stocks. They negotiate settlement terms over oversized pay packages and monitor social-media-driven price moves, acting after the fact through filings and fines rather than shaping the day-to-day valuation conversation.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, securities_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__musk_cult_believer, musk_control_block).
narrative_ontology:fixing_cost_class(valuation_legitimacy__musk_cult_believer, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Frontier technology programs burn cash for years before financial statements can justify them, and conventional metric-based valuation starves exactly such programs. The track-record criterion pools patient retail capital, sustains payroll and development through loss-making years, gives employees equity that retains them against cash-poor offers, and gives a dispersed community a shared reason to hold through drawdowns.
% TRANSFER_FUNCTION: Moves retirement savings and discretionary capital from believers and index holders into founder-controlled ventures at narrative-set prices; moves status and belonging to holders who stay and public ridicule to those who sell or doubt; periodically transfers forced losses to short sellers who must repurchase at narrative-set prices.
% ABSENT_VOICES: Minority shareholders' objections are present on ballots but nullified by the voting block; short sellers and independent analysts are present in the market yet excluded from the community spaces where the valuation conversation actually happens; actuarial and audit voices questioning trillion-dollar pay packages speak in filings few retail holders read.
% DISAPPEARANCE_RATIONALE: If track-record legitimacy vanished overnight, the ventures would face statement-based repricing: capital raises would compress, compensation packages would renegotiate against cash-flow benchmarks, the retail coordination would dissolve into ordinary fundamentals trading, and short interest would reprice immediately. Programs with genuine cash flows — constellation service, vehicle deliveries — would continue, but at valuations set by statements rather than story.
% FOUNDING_PROBLEM: Conventional valuation methods systematically failed to price genuinely novel technology programs: analysts declared reusable rockets impossible, doubted constellation economics, and missed the vehicle-production ramp, so backers needed a legitimacy basis that did not depend on metrics that kept saying no.
% FOUNDING_PROBLEM_CORROBORATION: Launch-manifest customers who pay to fly on reused boosters and telecom operators contracting constellation capacity attest the delivered capabilities from outside the believer community, as do analysts who publicly reversed earlier 'impossible' calls after milestones landed. Stated plainly: no party outside the benefiting set attests that the track record warrants the current valuation multiple — the conversions endorse past deliveries, not the forward extension of the criterion to the next tier of goals.
narrative_ontology:disappearance_verdict(valuation_legitimacy__musk_cult_believer, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__musk_cult_believer, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__musk_cult_believer, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(valuation_legitimacy__musk_cult_believer, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__musk_cult_believer, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(valuation_legitimacy__musk_cult_believer_tests).
:- end_tests(valuation_legitimacy__musk_cult_believer_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Metrics are authored from this reading's own lights over the standing arrangement, independently of the claimed type. Epsilon 0.40: the reading itself concedes a victim set (shorts, skeptics) and concedes that minority ballots are nullified, while rating those costs as deserved or negligible — hence moderate, not low. Suppression 0.62: the community actively maintains boundaries (removal of hostile content, harassment of bearish voices, reinterpretation of risk warnings as tactics), which the believer endorses as anti-manipulation hygiene but which is suppression nonetheless. Theater 0.22: the underlying engineering is real — boosters land and fly again — so performance is a minority share of activity, concentrated in milestone celebration and countdown ritual. Accessibility_collapse 0.28: rival criteria remain fully available (discounted-cash-flow models, shorting, other assets); nothing collapses when the track-record criterion is understood, it merely loses the argument inside the community. Resistance 0.72: institutional skepticism, governance campaigns, and regulatory friction persist and periodically surge. Temporal shape: the series ratchet upward across drawdown-revindication cycles rather than oscillating symmetrically — each cycle (2018-19 near-bankruptcy to 2020-21 squeeze vindication; 2022 drawdown to 2023-24 recovery) leaves enforcement infrastructure in place, so suppression steps up during drawdowns (2022 spike) and relaxes only partially at vindication peaks. Base properties are measured at interval end (2025), in the post-vindication consolidation phase.
 *
 * PERSPECTIVAL GAP:
 *   The believer seat and the payer seats compute different types from identical facts: from inside the community the arrangement is coordination it proudly sustains (approaching a rope profile), while from the short seller's and governance investor's seats the same structure operates as enforced extraction (snare-flavored). The sharpest divergence is WITHIN the beneficiary seat: the 2013-2019 entry cohort holds deep gains and experiences the regime as subsidy, while the 2021-and-later cohort carries high cost basis and experiences the same regime as exposure — same nominal seat, materially different directionality. Analysts occupy a doubled position: present in the market, excluded from the community, so the same published work is professional output in one venue and purged noise in the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: musk_control_block and musk_affiliated_ventures sit near the beneficiary pole (the regime subsidizes their capital access); buy_and_hold_believers derive low d from beneficiary status, pulled slightly toward target by identity-locked exit — they cannot leave without cost, and trapped participants sit nearer the target end than mobile ones. short_sellers sit near the full-target end with constrained exit amplifying their effective burden (covering means realizing the loss the narrative priced). institutional_governance_investors carry high d but mobile exit damps it. passive_index_holders bear diffuse costs with constrained exit — mid-to-high d despite their passive posture. independent_analysts_and_skeptics carry high d with mobile exit damping. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms produce the correct relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — metric-based valuation starving genuinely novel technology programs — was real and, from this reading's seat, remains live: each new program outruns its statements for years. Founding_problem_status=live paired with disappearance_verdict=world_rearranges produces no zombie/mandatrophy flag, and none is warranted: the arrangement has not outlived its function from any seat in this story. The tangled_rope claim is what prevents mislabeling in both directions: reading the arrangement as pure snare erases the genuine coordination (patient capital through loss years, employee retention via equity, community liquidity through drawdowns); reading it as pure rope erases the enforced asymmetry (nullified minority ballots, squeezed shorts, purged skeptics, compensation milestones defined by the narrative's own author). Identity-lock dynamics: the believer seat fuses relational identity (community belonging), ideological identity (multiplanetary destiny, accelerationism), and biographical sunk cost (life savings framed as participation in history). If that frame broke — a visible failure cascade the interpretive layer could not absorb — believers would flip toward the target end, epsilon would jump, and the classification would drift toward snare; the omega variables survivorship_base_rate_warrant and reflexive_inflow_dependence carry that contingency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is the track-record criterion the correct instantiation of valuation legitimacy, or does one of the sibling readings (discounted proven cash flows, technological option space, minority-protecting governance) hold the kernel?',
    'Full-market-cycle adjudication: which criterion''s holders end the cycle with realized returns and surviving institutions, and which reading''s predictions were falsified by disclosed outcomes.',
    'Adopting a sibling reading changes the victim set and epsilon materially — the governance_skeptic reading makes minority shareholders the primary victims and the control block the extractor; the dcf_fundamentalist reading converts the entire narrative premium into unpriced risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the valuation_legitimacy kernel this arrangement actually instantiates.').

omega_variable(
    survivorship_base_rate_warrant,
    'Does one founder''s delivered ''impossibilities'' license extrapolation to the next tier of goals, or is the track record a survivorship artifact that regresses toward base rates at larger scale?',
    'Base-rate study of public ''impossible goal'' commitments versus delivery across comparable founders, plus a within-founder audit of missed commitments (autonomy timelines, tunnel throughput, announced products never shipped).',
    'If base rates regress, the believer seat flips from beneficiary toward victim of the arrangement it sustains, epsilon rises sharply, and the classification drifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survivorship_base_rate_warrant, empirical, 'Whether track-record extrapolation is warranted inference or survivorship bias.').

omega_variable(
    reflexive_inflow_dependence,
    'Are believers'' gains backed by terminal venture cash flows, or are they contingent on continued recruitment of new capital at ever-higher narrative-set prices?',
    'Decompose holding-period returns into cash-flow yield versus multiple expansion funded by new inflows; test whether returns decouple from inflow growth as the ventures mature.',
    'If inflow-dependent, the beneficiary seat is transitional and the arrangement carries reflexive-recruitment risk; if cash-flow-backed, the coordination half of the tangled-rope reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reflexive_inflow_dependence, empirical, 'Whether believer returns are cash-flow-backed or recruitment-contingent.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of dissent inside the believer community structural (moderation practices, coordinated harassment, platform dynamics) or internalized (self-censorship, doubt experienced as betrayal of the mission)?',
    'Post-exit trajectory study of former believers: if skeptical analysis resumes freely after selling, suppression was largely internalized; if fear of harassment and community loss persists after exit, it was structural.',
    'Internalized suppression travels with the holder after exit, making effective suppression higher than the structural measure suggests and deepening the identity lock that stabilizes the whole arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of dissent among believers.').

omega_variable(
    tactic_vs_genuine_risk_warnings,
    'Are the bankruptcy and risk warnings this reading reinterprets as negotiating tactics actually genuine risk assessments that the reading''s interpretive layer absorbs and discards?',
    'Ex-post audit of warned risks: which materialized, on what timeline, and whether the ''tactic'' interpretation predicted the warnings'' timing better than a risk-assessment interpretation.',
    'If the warnings were genuine, the reading''s own seat bears unrecognized risk, epsilon for the believer seat rises, and the interpretive layer''s authority — the move that makes ''metrics are lagging'' unfalsifiable — weakens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tactic_vs_genuine_risk_warnings, empirical, 'Whether dismissed risk warnings were tactical rhetoric or genuine assessments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__musk_cult_believer, 2013, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t2013, valuation_legitimacy__musk_cult_believer, theater_ratio, 2013, 0.1).
narrative_ontology:measurement_basis(valu_tr_t2013, observed).
narrative_ontology:measurement(valu_tr_t2015, valuation_legitimacy__musk_cult_believer, theater_ratio, 2015, 0.12).
narrative_ontology:measurement_basis(valu_tr_t2015, observed).
narrative_ontology:measurement(valu_tr_t2017, valuation_legitimacy__musk_cult_believer, theater_ratio, 2017, 0.14).
narrative_ontology:measurement_basis(valu_tr_t2017, observed).
narrative_ontology:measurement(valu_tr_t2019, valuation_legitimacy__musk_cult_believer, theater_ratio, 2019, 0.15).
narrative_ontology:measurement_basis(valu_tr_t2019, observed).
narrative_ontology:measurement(valu_tr_t2020, valuation_legitimacy__musk_cult_believer, theater_ratio, 2020, 0.17).
narrative_ontology:measurement_basis(valu_tr_t2020, observed).
narrative_ontology:measurement(valu_tr_t2021, valuation_legitimacy__musk_cult_believer, theater_ratio, 2021, 0.2).
narrative_ontology:measurement_basis(valu_tr_t2021, observed).
narrative_ontology:measurement(valu_tr_t2022, valuation_legitimacy__musk_cult_believer, theater_ratio, 2022, 0.18).
narrative_ontology:measurement_basis(valu_tr_t2022, observed).
narrative_ontology:measurement(valu_tr_t2023, valuation_legitimacy__musk_cult_believer, theater_ratio, 2023, 0.2).
narrative_ontology:measurement_basis(valu_tr_t2023, observed).
narrative_ontology:measurement(valu_tr_t2025, valuation_legitimacy__musk_cult_believer, theater_ratio, 2025, 0.22).
narrative_ontology:measurement_basis(valu_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(valu_be_t2013, valuation_legitimacy__musk_cult_believer, base_extractiveness, 2013, 0.18).
narrative_ontology:measurement_basis(valu_be_t2013, observed).
narrative_ontology:measurement(valu_be_t2015, valuation_legitimacy__musk_cult_believer, base_extractiveness, 2015, 0.22).
narrative_ontology:measurement_basis(valu_be_t2015, observed).
narrative_ontology:measurement(valu_be_t2017, valuation_legitimacy__musk_cult_believer, base_extractiveness, 2017, 0.26).
narrative_ontology:measurement_basis(valu_be_t2017, observed).
narrative_ontology:measurement(valu_be_t2019, valuation_legitimacy__musk_cult_believer, base_extractiveness, 2019, 0.29).
narrative_ontology:measurement_basis(valu_be_t2019, observed).
narrative_ontology:measurement(valu_be_t2020, valuation_legitimacy__musk_cult_believer, base_extractiveness, 2020, 0.33).
narrative_ontology:measurement_basis(valu_be_t2020, observed).
narrative_ontology:measurement(valu_be_t2021, valuation_legitimacy__musk_cult_believer, base_extractiveness, 2021, 0.37).
narrative_ontology:measurement_basis(valu_be_t2021, observed).
narrative_ontology:measurement(valu_be_t2022, valuation_legitimacy__musk_cult_believer, base_extractiveness, 2022, 0.35).
narrative_ontology:measurement_basis(valu_be_t2022, observed).
narrative_ontology:measurement(valu_be_t2023, valuation_legitimacy__musk_cult_believer, base_extractiveness, 2023, 0.38).
narrative_ontology:measurement_basis(valu_be_t2023, observed).
narrative_ontology:measurement(valu_be_t2025, valuation_legitimacy__musk_cult_believer, base_extractiveness, 2025, 0.4).
narrative_ontology:measurement_basis(valu_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(valu_su_t2013, valuation_legitimacy__musk_cult_believer, suppression_requirement, 2013, 0.35).
narrative_ontology:measurement_basis(valu_su_t2013, observed).
narrative_ontology:measurement(valu_su_t2015, valuation_legitimacy__musk_cult_believer, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement_basis(valu_su_t2015, observed).
narrative_ontology:measurement(valu_su_t2017, valuation_legitimacy__musk_cult_believer, suppression_requirement, 2017, 0.44).
narrative_ontology:measurement_basis(valu_su_t2017, observed).
narrative_ontology:measurement(valu_su_t2019, valuation_legitimacy__musk_cult_believer, suppression_requirement, 2019, 0.48).
narrative_ontology:measurement_basis(valu_su_t2019, observed).
narrative_ontology:measurement(valu_su_t2020, valuation_legitimacy__musk_cult_believer, suppression_requirement, 2020, 0.52).
narrative_ontology:measurement_basis(valu_su_t2020, observed).
narrative_ontology:measurement(valu_su_t2021, valuation_legitimacy__musk_cult_believer, suppression_requirement, 2021, 0.55).
narrative_ontology:measurement_basis(valu_su_t2021, observed).
narrative_ontology:measurement(valu_su_t2022, valuation_legitimacy__musk_cult_believer, suppression_requirement, 2022, 0.6).
narrative_ontology:measurement_basis(valu_su_t2022, observed).
narrative_ontology:measurement(valu_su_t2023, valuation_legitimacy__musk_cult_believer, suppression_requirement, 2023, 0.58).
narrative_ontology:measurement_basis(valu_su_t2023, observed).
narrative_ontology:measurement(valu_su_t2025, valuation_legitimacy__musk_cult_believer, suppression_requirement, 2025, 0.62).
narrative_ontology:measurement_basis(valu_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__musk_cult_believer, identity_coordination).
narrative_ontology:affects_constraint(valuation_legitimacy__musk_cult_believer, valuation_legitimacy__dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__musk_cult_believer, valuation_legitimacy__real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__musk_cult_believer, valuation_legitimacy__governance_skeptic).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial question 'is the valuation justified?' covers four structurally distinct claims about what grounds valuation legitimacy. Each reading is a separate story with its own epsilon, beneficiary/victim structure, and classification over the SAME standing arrangement (narrative-priced founder-controlled ventures). This reading's market dominance changes the operating environment of the siblings — believer buying pressure and narrative dominance alter the legitimacy conditions under which dcf_fundamentalist and governance_skeptic arguments get heard, and supply the constituency real_options_technologist reasons for — so edges run from this story to all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
