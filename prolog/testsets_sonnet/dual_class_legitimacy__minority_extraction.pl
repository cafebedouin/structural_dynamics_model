% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__minority_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_class_legitimacy__minority_extraction, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: dual_class_legitimacy__minority_extraction
 *   human_readable: Dual-Class Share Structure — Minority Governance Extraction Reading
 *   domain: corporate_governance/securities_law
 *
 * SUMMARY:
 *   This story instantiates the minority_extraction reading of the
 *   dual-class-legitimacy kernel: it holds that voting power should track
 *   capital contribution and risk exposure, and that the persistent gap
 *   between founder vote-share and founder capital-share is a transfer of
 *   governance value from public shareholders to insiders, not a legitimate
 *   stewardship arrangement. The sibling readings (founder_stewardship, which
 *   holds concentrated control serves all shareholders through long-horizon
 *   execution; disclosure_consent, which holds legitimacy rests on informed
 *   consent at purchase rather than on control parity) are NOT represented in
 *   this file's metrics or classification — they are separate constraints
 *   with their own ε values, to be authored separately and linked via
 *   network.affects_constraints. This file's claimed_type, extractiveness,
 *   and suppression describe only the minority-extraction reading's
 *   structural claim: that dual-class structure with controlled-company
 *   exemptions is a Tangled Rope — it does coordinate genuine long-horizon
 *   commitment (Boltzmann floor acknowledged) but extracts governance value
 *   from Class A holders through active, ratcheting enforcement (listing
 *   exemptions, charter terms, proxy structure) that a purely coordinative
 *   arrangement would not require.
 *
 * KEY AGENTS:
 *   - founder_control_bloc: Primary beneficiary (institutional/arbitrage) — sets governance terms, retains supervoting shares while diversifying economic exposure
 *   - early_venture_investors: Secondary beneficiary (powerful/arbitrage) — negotiated preferential terms pre-IPO, largely exited by time of public extraction
 *   - class_a_public_shareholders: Primary target (powerless/constrained) — bears majority of capital risk, minority of votes
 *   - index_fund_beneficial_owners: Diffuse trapped target (powerless/trapped) — passive extraction victims via mandatory index holding
 *   - stock_exchanges_and_regulators: Institutional agenda-setter/observer — sets the listing-standard exemptions that permit the structure to persist
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__minority_extraction, 0.71).
domain_priors:suppression_score(dual_class_legitimacy__minority_extraction, 0.68).
domain_priors:theater_ratio(dual_class_legitimacy__minority_extraction, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, extractiveness, 0.71).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__minority_extraction, tangled_rope).
narrative_ontology:human_readable(dual_class_legitimacy__minority_extraction, "Dual-Class Share Structure — Minority Governance Extraction Reading").
narrative_ontology:topic_domain(dual_class_legitimacy__minority_extraction, "corporate_governance/securities_law").

domain_priors:requires_active_enforcement(dual_class_legitimacy__minority_extraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__minority_extraction, 'a8b73e6c-b87c-4f12-aa34-48d9d0dfe11c').
narrative_ontology:cs_kernel_codification('a8b73e6c-b87c-4f12-aa34-48d9d0dfe11c', formalized).
narrative_ontology:cs_authority_grounding('a8b73e6c-b87c-4f12-aa34-48d9d0dfe11c', extraction).
narrative_ontology:cs_interpretation_layer_present('a8b73e6c-b87c-4f12-aa34-48d9d0dfe11c').
narrative_ontology:cs_reading_relation('a8b73e6c-b87c-4f12-aa34-48d9d0dfe11c', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('a8b73e6c-b87c-4f12-aa34-48d9d0dfe11c', dual_class_legitimacy__disclosure_consent, coexists_with).
narrative_ontology:cs_axiom('a8b73e6c-b87c-4f12-aa34-48d9d0dfe11c', foundational, voice_must_track_capital_and_risk).
narrative_ontology:cs_axiom_status(voice_must_track_capital_and_risk, holdable).
narrative_ontology:cs_axiom_grounding('a8b73e6c-b87c-4f12-aa34-48d9d0dfe11c', voice_must_track_capital_and_risk, deontological).
narrative_ontology:cs_axiom('a8b73e6c-b87c-4f12-aa34-48d9d0dfe11c', secondary, indefinite_control_absent_sunset_is_entrenchment).
narrative_ontology:cs_axiom_status(indefinite_control_absent_sunset_is_entrenchment, holdable).
narrative_ontology:cs_axiom_grounding('a8b73e6c-b87c-4f12-aa34-48d9d0dfe11c', indefinite_control_absent_sunset_is_entrenchment, empirically_contingent).
narrative_ontology:cs_reference_frame('a8b73e6c-b87c-4f12-aa34-48d9d0dfe11c', one_share_one_vote_baseline).
narrative_ontology:cs_drift_state('a8b73e6c-b87c-4f12-aa34-48d9d0dfe11c', post_tech_ipo_wave_2010s_2020s, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a8b73e6c-b87c-4f12-aa34-48d9d0dfe11c', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__minority_extraction, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, founder_control_bloc).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, early_venture_investors).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, class_a_public_shareholders).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, index_fund_beneficial_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds high-vote Class B/founder shares carrying 10-20x the voting power of publicly traded Class A shares while contributing a small and often shrinking fraction of total capital at risk. Sets board composition, approves or blocks all major corporate actions (mergers, charter amendments, executive compensation), and negotiates controlled-company exemptions from exchange listing rules that would otherwise require independent board majorities and committees. Bears personal wealth concentration risk but retains liquidity through secondary sales and can sell down economic exposure while retaining voting control.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, founder_control_bloc, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__minority_extraction, founder_control_bloc, beneficiary).

% Negotiated preferential share classes and board seats pre-IPO in exchange for early capital at high risk; largely exited or diversified by the time public shareholders enter, but retain governance-friendly terms (ratchets, protective provisions) that were priced into the founder-control bargain and that public buyers do not receive.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, early_venture_investors, beneficiary,
    powerful, biographical, arbitrage, national).

% Purchase Class A shares on public exchanges, contributing the substantial majority of the company's market capitalization and bearing full economic downside risk, but hold votes with a fraction of the weight of founder shares. Cannot elect a majority of directors, cannot block related-party transactions or executive pay packages, and cannot force a sunset on the dual-class structure through ordinary shareholder process. Exit is limited to selling shares — which crystallizes losses without changing the governance structure for anyone else.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, class_a_public_shareholders, payer,
    powerless, biographical, constrained, national).

% Millions of retirement savers and retail investors hold Class A economic exposure indirectly through index funds that are mandated to hold the stock regardless of governance terms because the company sits in a market-cap-weighted index. They bear the extraction passively, have no realistic exit (divesting means leaving the index, which fund mandates forbid), and rarely know the mechanism exists.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, index_fund_beneficial_owners, payer,
    powerless, generational, trapped, global).

% Index providers (S&P, FTSE Russell) have at times excluded new dual-class listings from major indices in response to governance criticism, and proxy advisors (ISS, Glass Lewis) recommend against dual-class perpetuation, but neither can force a recapitalization; their objections are advisory and are frequently overridden once a company is grandfathered into an index or once founder control blocks the relevant votes.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, index_providers_and_proxy_advisors, excluded,
    organized, biographical, constrained, global).

% NYSE, Nasdaq, and the SEC set the listing standards that define which corporate-governance protections dual-class and controlled companies may waive. They have historically permitted broad controlled-company exemptions (from independent board/committee majority requirements) as a competitive concession to attract listings, and their rulemaking directly determines how much minority protection survives the structure.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, stock_exchanges_and_regulators, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__minority_extraction, stock_exchanges_and_regulators, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Dual-class structures do coordinate something real: they let a founding team commit to a long-term strategy without being forced into short-horizon capital-market discipline at every quarterly cycle, and they let early risk-capital providers lock in governance terms proportional to the risk they bore before the company had public market validation.
% TRANSFER_FUNCTION: Moves governance value — the ability to set strategy, approve transactions, set executive pay, and block takeovers — from the shareholders who supply the majority of the capital and bear the majority of the downside risk (Class A public holders) to the shareholders who supply a minority of capital but hold supermajority votes (founder/insider class).
% ABSENT_VOICES: Public shareholders as a class have no seat at the table when dual-class terms are set (they are set pre-IPO, unilaterally, by the founder and early investors as a condition of the offering) and no mechanism to renegotiate them post-IPO short of a founder-initiated sunset or an exceedingly rare successful activist campaign. Index fund beneficial owners are doubly absent: their intermediary funds cannot divest without leaving benchmark mandates, and the underlying retail savers rarely know the structure exists.
% DISAPPEARANCE_RATIONALE: If dual-class structures and controlled-company exemptions vanished overnight, board composition would shift toward independent majorities, executive pay and related-party transactions would face binding shareholder votes, hostile and friendly acquisition dynamics would change substantially (many controlled companies are effectively acquisition-proof), and founder wealth concentration currently defended by supervoting shares would become contestable through ordinary capital markets processes. This is not a cosmetic constraint — real governance authority currently sits with a class of shareholders holding a minority economic stake.
% FOUNDING_PROBLEM: Founders taking a company public feared that dispersed public shareholders and activist investors would force short-term decision-making, block visionary long-horizon bets, or enable a hostile takeover that would strip the company of its founding mission before it could be executed.
% FOUNDING_PROBLEM_CORROBORATION: Founders and their venture backers attest the problem remains live, citing activist-investor short-termism and hostile-takeover risk as ongoing threats requiring insulated control. Independent corporate-governance researchers (Bebchuk & Kastiel and others outside any benefiting party), the Council of Institutional Investors, and proxy advisory firms attest that empirical evidence on long-horizon benefit is mixed at best, that the mechanism persists and often widens years after any plausible 'protect the founding vision' rationale has expired, and that the structure functions primarily as permanent entrenchment rather than a time-limited shield.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__minority_extraction, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__minority_extraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__minority_extraction, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dual_class_legitimacy__minority_extraction, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__minority_extraction, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__minority_extraction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dual_class_legitimacy__minority_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dual_class_legitimacy__minority_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.71 by interval end because the vote-to-capital gap in mature dual-class companies typically widens over time as founders sell down economic stakes while retaining supervoting shares — the mechanism transfers proportionally more governance value per dollar of capital as the founder's capital share shrinks. Suppression is authored at 0.68: it is a raw, unscaled structural property reflecting that public shareholders have no ordinary-process mechanism (no binding say-on-structure vote, no cumulative voting remedy) to compel a sunset; suppression here is the absence of exit-via-voice, not the presence of exit-via-sale (selling shares is available but does not change the structure for remaining holders, so it is not a real remedy for the class). Theater ratio rises to 0.42, reflecting that a growing share of governance activity (independent director appointments, ESG committees) in controlled companies has become performative given that founder votes can override any recommendation these bodies produce — theater layered onto a persisting core extraction, not a metric-substitution mountain claim.
 *
 * PERSPECTIVAL GAP:
 *   The founder_control_bloc seat and the class_a_public_shareholders seat compute structurally differently from identical financial disclosures: from the control bloc's seat, the arrangement is a bargained-for, disclosed feature of the security they purchased knowingly (a rope/coordination framing); from the public shareholder seat, the same structure operates as an enforced, ratcheting transfer they have no voice-based remedy against (extraction framing). This story deliberately claims tangled_rope rather than snare because a genuine coordination function (insulation from short-termism) is present alongside the extraction — a pure snare claim would erase the coordination story's partial legitimacy, which is exactly the analytical work the minority_extraction reading needs to do honestly rather than as advocacy.
 *
 * DIRECTIONALITY LOGIC:
 *   founder_control_bloc and early_venture_investors are declared beneficiaries: they hold governance rights disproportionate to current capital-at-risk and can exit economic exposure while retaining voting control (arbitrage exit), pushing their derived directionality toward the beneficiary end. class_a_public_shareholders and index_fund_beneficial_owners are declared victims: they bear the marginal capital risk without commensurate voice, and their exit options (constrained/trapped) push derived directionality toward the full-target end — index fund holders especially, since benchmark-mandate lock-in removes even the sell-and-exit remedy available to direct retail holders.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protect long-horizon mission execution from short-term capital-market pressure) may have been genuinely live at IPO but the founding_problem_status is authored as contested rather than dead, because credible corroboration exists on both sides. The tangled_rope classification (rather than snare) is what prevents this story from mislabeling all dual-class structure as pure extraction — the coordination function is real and independently attested even by critics; what makes it tangled rather than a clean rope is that the structure lacks any binding time-limit or performance-linked sunset, so the same mechanism that legitimately shields early strategy execution persists indefinitely as insider entrenchment once the original risk asymmetry (founder bearing outsized risk pre-IPO) has inverted (founder de-risked via diversification, public shareholders now bearing the outsized risk).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Is the minority_extraction reading, the founder_stewardship reading, or the disclosure_consent reading the structurally correct account of dual-class legitimacy — or do different companies at different lifecycle stages instantiate different readings simultaneously?',
    'This is the committer-axis contest for the dual_class_legitimacy kernel. A sibling reading (founder_stewardship) would hold the SAME structural facts (vote/capital gap, controlled-company exemptions) constitute legitimate stewardship rather than extraction, on the premise that long-horizon mission execution benefits all shareholders including the ones who lack voice. Another sibling (disclosure_consent) would hold that legitimacy is fully established at the point of informed purchase under Securities Act disclosure obligations, making the post-purchase governance gap irrelevant to legitimacy as long as it was disclosed. These are not measurement disagreements about this constraint — they are three different constraints reading the same underlying kernel (concentrated founder control via differential voting rights) differently. Each should be authored as its own story with its own ε and stakeholders; this file deliberately does not average or hedge across them.',
    'If the founder_stewardship reading is adopted instead, the same facts would likely classify as rope or scaffold (coordination-dominant, time-limited by founder tenure) rather than tangled_rope; if the disclosure_consent reading is adopted, the classification question shifts entirely away from the governance-gap metric toward disclosure-adequacy metrics, potentially reading as a near-mountain (settled by informed consent, minimal ongoing extraction claim).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Which reading of the dual-class-legitimacy kernel governs — the disagreement is located in whether the vote/capital gap itself is the extractive fact, or whether informed pre-purchase consent or long-horizon aggregate benefit displaces that fact''s normative weight.').

omega_variable(
    sunset_absence_significance,
    'Does the near-universal absence of binding time-based or performance-based sunset provisions in dual-class charters indicate the structure was never intended as transitional (supporting the extraction reading), or does it reflect founders'' good-faith belief that mission-critical control needs indefinite duration (supporting the stewardship reading)?',
    'Comparative study of the minority of dual-class companies that DO adopt sunset provisions (time-based or founder-departure-triggered) versus those that do not — track whether extraction metrics (governance gap indices, related-party transaction frequency, executive pay divergence) differ systematically between sunset and no-sunset firms.',
    'If sunset-provision firms show materially lower extraction metrics without worse strategic outcomes, this supports classifying no-sunset dual-class structures more toward snare (the coordination benefit could have been achieved with time-limited protection, so the indefinite duration is excess extraction); if outcomes are similar, this supports the founder_stewardship reading that indefinite control genuinely tracks an indefinite legitimate need.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_absence_significance, empirical, 'Whether the general absence of sunset clauses in dual-class structures is diagnostic of extraction intent or reflects genuine open-ended stewardship need.').

omega_variable(
    controlled_company_exemption_necessity,
    'Are the specific listing-standard exemptions granted to controlled companies (waiver of independent board/committee majority requirements) necessary complements to dual-class voting structure, or a separable, additional layer of extraction stacked on top of the voting mechanism?',
    'Compare governance and financial outcomes of dual-class companies that voluntarily maintain independent board/committee majorities against those that fully exercise the controlled-company exemption, controlling for industry and lifecycle stage.',
    'If voluntary independent-board dual-class companies show no worse strategic flexibility, this indicates the exemption layer is separable excess extraction beyond what founder control itself requires — strengthening the tangled_rope (or even snare) classification of the exemption specifically, apart from the voting structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(controlled_company_exemption_necessity, empirical, 'Whether controlled-company board exemptions are a necessary complement to supervoting shares or a stackable additional extraction layer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__minority_extraction, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__minority_extraction, theater_ratio, 0, 0.2).
narrative_ontology:measurement(dual_tr_t4, dual_class_legitimacy__minority_extraction, theater_ratio, 4, 0.26).
narrative_ontology:measurement(dual_tr_t8, dual_class_legitimacy__minority_extraction, theater_ratio, 8, 0.31).
narrative_ontology:measurement(dual_tr_t12, dual_class_legitimacy__minority_extraction, theater_ratio, 12, 0.35).
narrative_ontology:measurement(dual_tr_t16, dual_class_legitimacy__minority_extraction, theater_ratio, 16, 0.39).
narrative_ontology:measurement(dual_tr_t20, dual_class_legitimacy__minority_extraction, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__minority_extraction, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(dual_be_t4, dual_class_legitimacy__minority_extraction, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(dual_be_t8, dual_class_legitimacy__minority_extraction, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(dual_be_t12, dual_class_legitimacy__minority_extraction, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(dual_be_t16, dual_class_legitimacy__minority_extraction, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(dual_be_t20, dual_class_legitimacy__minority_extraction, base_extractiveness, 20, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__minority_extraction, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(dual_su_t4, dual_class_legitimacy__minority_extraction, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(dual_su_t8, dual_class_legitimacy__minority_extraction, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(dual_su_t12, dual_class_legitimacy__minority_extraction, suppression_requirement, 12, 0.63).
narrative_ontology:measurement(dual_su_t16, dual_class_legitimacy__minority_extraction, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(dual_su_t20, dual_class_legitimacy__minority_extraction, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__minority_extraction, resource_allocation).
narrative_ontology:boltzmann_floor_override(dual_class_legitimacy__minority_extraction, 0.12).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the natural-language claim 'dual-class share structures with concentrated founder control are legitimate/illegitimate.' Per the epsilon-invariance principle, this label conflates at least three structurally distinct legitimacy claims that would carry different ε if measured against different normative baselines: (1) minority_extraction (this story) measures against a capital/risk-proportional voice baseline and finds substantial, actively-enforced extraction (tangled_rope, epsilon~0.71); (2) founder_stewardship measures against a mission-continuity/all-shareholder-welfare baseline and would likely find the same structural facts consistent with rope or scaffold; (3) disclosure_consent measures against an informed-consent-at-purchase baseline and would likely find low ongoing extraction once adequate disclosure is established. All three stories share the same underlying institutional facts (vote/capital ratios, controlled-company exemptions) but apply different normative frames that the kernel leaves genuinely contested — hence three separate constraint files rather than one hedged story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
