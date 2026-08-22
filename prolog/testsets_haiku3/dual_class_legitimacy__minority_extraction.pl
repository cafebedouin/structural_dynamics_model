% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__minority_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: dual_class_legitimacy__minority_extraction
 *   human_readable: Dual-Class Share Control and Governance Extraction from Minority Shareholders
 *   domain: corporate_governance/securities_law
 *
 * SUMMARY:
 *   This constraint is one reading of the contested kernel of dual-class
 *   legitimacy. This reading — minority_extraction — instantiates the claim
 *   that minority shareholders are entitled to governance proportional to
 *   capital and risk borne, and that dual-class share structures violate that
 *   entitlement by transferring governance value to founder-held shares. The
 *   referent is the standing arrangement: dual-class voting as currently
 *   practiced in US listed companies. This reading assesses that arrangement
 *   as substantially extractive from the vantage of Class B shareholders who
 *   bear risk without voice. Sibling readings (founder_stewardship and
 *   disclosure_consent) offer alternative framings of the same kernel — each
 *   reading defines its own ε, beneficiary/victim structure, and
 *   classification.
 *
 * KEY AGENTS:
 *   - founder_controlled_class_a: Sets and enforces dual-class voting rules; benefits from governance extraction without corresponding capital concentration
 *   - public_minority_shareholders: Hold majority of capital and all equity risk (price, fundamental); denied governance proportional to risk and capital
 *   - board_of_directors: Executes founder will; legitimate independence is theater when founder voting prevents override
 *   - securities_regulators: Permit dual-class via disclosure but exempt controlled companies from protective regulations
 *   - activist_investors: Locked out of governance despite capital concentration; exclusion is the enforcement object
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__minority_extraction, 0.79).
domain_priors:suppression_score(dual_class_legitimacy__minority_extraction, 0.71).
domain_priors:theater_ratio(dual_class_legitimacy__minority_extraction, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, extractiveness, 0.79).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__minority_extraction, tangled_rope).
narrative_ontology:human_readable(dual_class_legitimacy__minority_extraction, "Dual-Class Share Control and Governance Extraction from Minority Shareholders").
narrative_ontology:topic_domain(dual_class_legitimacy__minority_extraction, "corporate_governance/securities_law").

domain_priors:requires_active_enforcement(dual_class_legitimacy__minority_extraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__minority_extraction, '5eddcc02-b4c7-4a34-9490-25688d5b1ced').
narrative_ontology:cs_kernel_codification('5eddcc02-b4c7-4a34-9490-25688d5b1ced', formalized).
narrative_ontology:cs_authority_grounding('5eddcc02-b4c7-4a34-9490-25688d5b1ced', extraction).
narrative_ontology:cs_interpretation_layer_present('5eddcc02-b4c7-4a34-9490-25688d5b1ced').
narrative_ontology:cs_reading_relation('5eddcc02-b4c7-4a34-9490-25688d5b1ced', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('5eddcc02-b4c7-4a34-9490-25688d5b1ced', dual_class_legitimacy__disclosure_consent, influences).
narrative_ontology:cs_axiom('5eddcc02-b4c7-4a34-9490-25688d5b1ced', foundational, governance_rights_proportional_to_capital_risk).
narrative_ontology:cs_axiom_status(governance_rights_proportional_to_capital_risk, holdable).
narrative_ontology:cs_axiom_grounding('5eddcc02-b4c7-4a34-9490-25688d5b1ced', governance_rights_proportional_to_capital_risk, deontological).
narrative_ontology:cs_axiom('5eddcc02-b4c7-4a34-9490-25688d5b1ced', secondary, dual_class_violates_shareholder_parity).
narrative_ontology:cs_axiom_status(dual_class_violates_shareholder_parity, holdable).
narrative_ontology:cs_axiom_grounding('5eddcc02-b4c7-4a34-9490-25688d5b1ced', dual_class_violates_shareholder_parity, empirically_contingent).
narrative_ontology:cs_reference_frame('5eddcc02-b4c7-4a34-9490-25688d5b1ced', one_share_one_vote_principle).
narrative_ontology:cs_drift_state('5eddcc02-b4c7-4a34-9490-25688d5b1ced', contemporary_public_markets, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5eddcc02-b4c7-4a34-9490-25688d5b1ced', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__minority_extraction, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, founder_controlled_class_a).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, public_minority_shareholders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, board_of_directors).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, institutional_fiduciaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Founder-held shares carry 10-to-1 (or higher) voting rights per share relative to public Class B shares. Sets board composition, approves major transactions, and retains control through majority voting despite holding minority capital. Justifies structure as protecting long-term vision from short-term market pressure and enabling alignment with founder's mission. Can exit by selling shares while retaining control via remaining super-voting shares.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, founder_controlled_class_a, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold Class B shares with equal voting per share, but disproportionate founder Class A voting prevents any governance voice despite holding majority of capital. Bear equity risk (price volatility, fundamental business risk) without governance authority. Can sell shares on public market but cannot influence board or major decisions while holding. Lack protective provisions available to private company shareholders (drag-along, tag-along, information rights).
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, public_minority_shareholders, payer,
    organized, biographical, constrained, global).

% Composed primarily of founder-aligned directors or founder-selected independents. Operates under founder veto of substantive decisions. Benefits from association with founder/mission and faces reputational/legal risk if acting against founder interests. Board structure serves legitimacy function (independent committees, governance disclosures) but lacks real decision authority when founder voting prevents override.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, board_of_directors, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__minority_extraction, board_of_directors, agenda_setter).

% Enforce disclosure rules and listed-company requirements but grant controlled-company exemptions from proxy access, say-on-pay, and independent committee mandates. Can issue guidance but have limited enforcement leverage over governance structure itself once dual-class issuance occurs. Operate within securities law framework that permits dual-class but contains it through disclosure.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, securities_regulators, observer,
    institutional, generational, analytical, national).

% Formally independent from founder but lack meaningful leverage over founder-directed decisions. Can voice objections, abstain, or resign; cannot override founder preference through voting. Board seat is contingent on founder acceptance. Excluded from real governance function by founder super-voting regardless of formal independence title.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, independent_directors, excluded,
    moderate, biographical, mobile, global).

% Can accumulate large Class B stakes but have zero board seats or veto rights; cannot trigger proxy contests or board removal because founder control prevents shareholder voting from changing control. Structurally excluded from governance despite capital concentration. This exclusion is the enforcement object itself.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, activist_investors, excluded,
    powerful, immediate, constrained, global).

% Pension funds, endowments, and mutual funds hold Class B shares as part of index or diversified portfolios. Bear fiduciary duty to beneficiaries but have no governance avenue to protect beneficiary interests; locked into passive ownership by market structure. Lobby for governance reforms but cannot exit specific positions without portfolio disruption.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, institutional_fiduciaries, payer,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__minority_extraction, founder_controlled_class_a).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__minority_extraction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The dual-class structure coordinates founder retention and long-horizon business strategy: concentrating decision authority prevents short-termism and allows the founder to pursue multi-decade initiatives without quarterly earnings pressure.
% TRANSFER_FUNCTION: Transfers governance value (board seat power, veto rights, strategic control) from public minority shareholders to founder-held Class A shares, maintaining that concentration despite public capital raising. Class B shareholders provide capital and bear equity risk but exercise governance only through exit (selling shares), not voice.
% ABSENT_VOICES: Would-be alternative founders, activist shareholders, and independent investors are structurally excluded: they cannot contest the board or acquire control regardless of capital concentration. Securities class-action attorneys argue for minority protection but are never in the governance decision room. Foreign regulators (e.g., European traditions) would contest the structure but have no enforcement lever.
% DISAPPEARANCE_RATIONALE: If dual-class voting and controlled-company exemptions disappeared, public shareholders would acquire governance voice proportional to capital, board composition would become contestable through proxy, and founder control would require ongoing shareholder consent rather than automatic super-voting. Business strategy, asset allocation, and possibly founder tenure would be subject to shareholder accountability.
% FOUNDING_PROBLEM: Early-stage capital raising: founders needed to raise public capital for growth without ceding control to dispersed public shareholders who lack long-term commitment. Dual-class structure solved the problem of retaining strategic autonomy while accessing public markets.
% FOUNDING_PROBLEM_CORROBORATION: Founders attest the founding problem is live: founders report pressure to manage quarterly earnings despite long-term strategy. Public shareholders and governance reformers attest the founding problem is substantially solved: public capital markets are mature, founder competence is not the binding constraint, and the arrangement now persists as rent extraction. Academic research from outside the benefiting parties (Gompers et al., Roe, others) documents that dual-class returns deteriorate over long periods and control correlates with founder rent-taking rather than superior strategy execution.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__minority_extraction, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__minority_extraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__minority_extraction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dual_class_legitimacy__minority_extraction, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__minority_extraction, 0.79, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.79 at interval end) because the constraint transfers governance value (board seat power, veto authority, strategic control) from public shareholders to founders despite public shareholders' capital stake and risk bearing. The transfer is asymmetric and persistent — Class B shareholders cannot vote it away or exit into an alternative governance structure. Suppression is high (0.71) because the constraint's persistence depends on actively excluding shareholder proposals, proxy contests, and board removal through voting inequality — mechanisms that would otherwise be available. Theater is moderate-rising (0.41): board committees, independent directors, and say-on-pay votes serve legitimacy functions for public shareholders but lack real decision power when founder voting controls outcomes. The rising trajectory reflects accumulating theatrical governance activity (ESG reports, stakeholder committees) decoupled from real decision authority. The measurement series shared one time grid (shared endpoints across all three metrics) so temporal analysis is valid.
 *
 * PERSPECTIVAL GAP:
 *   From the founder seat, the dual-class structure is legitimate coordination: it solves the problem of raising public capital while preserving the founder's ability to execute long-term strategy without quarterly earnings pressure. From that seat, extracted governance value is the price public shareholders pay for access to founder-led companies. From the public shareholder seat, the same structure is pure extraction: founders use disproportionate voting rights to redirect strategic decisions toward founder preferences (e.g., personal charitable interests, empire-building, underperformance tolerance) at the expense of shareholder value. The engine computes these divergences from structural data — the payer seat and the agenda-setter seat should produce different type classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Founder Class A shares sit at d near 0.0 (full beneficiary): they collect governance value (control, strategic direction, veto power) without bearing proportional capital risk, and can exit by selling into the public market while retaining control. Public Class B shareholders sit at d near 1.0 (full target): they bear all equity risk (market volatility, fundamental business risk) but have zero governance voice, constrained exit (must sell as minority), and no protective provisions. The board sits at intermediate d but asymmetric: director compensation and reputational benefit flow from founder alignment, not from public shareholder value creation. Independent directors are trapped by founder veto despite formal independence titles — they sit at higher d than they should formally occupy because their actual authority is suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (solving capital constraint while preserving founder autonomy) was live at IPO but has evolved: founders today have abundant access to public capital without control sacrifice (secondary equity, debt markets, private equity alternatives). The arrangement now persists as rent collection and governance capture rather than solving a genuine coordination problem. Yet the constraint remains actively enforced: proxy access is blocked, independent directors lack real leverage, shareholder proposals are ignored, and the regulatory exemptions that permit this are maintained. This is the classic mandatrophy structure: the original mandate is dead or substantially served, but the extraction mechanism persists through institutional inertia and beneficiary control of the enforcement machinery. The high theater_ratio supports this — legitimacy activity (independent directors, governance disclosures, proxy voting procedures) is expanding while governance function is static or declining.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founder_stewardship_vs_extraction_dichotomy,
    'Does founder control produce superior long-term value creation (stewardship reading) or inferior value creation offset by founder private benefits (extraction reading)?',
    'Long-term performance comparison: dual-class companies vs. single-class peer cohorts, controlling for industry, size, and founding era. Measurement of founder''s private benefits extraction (private transactions, related-party deals, compensation, personal use of company assets).',
    'If stewardship: the governance concentration produces real value that justifies the extraction imposed on public shareholders, supporting reclassification toward rope or even mountain-adjacent. If extraction: the constraint is pure rent-seeking, confirming tangled_rope or snare classification and mandatrophy structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founder_stewardship_vs_extraction_dichotomy, empirical, 'Whether founder control creates shareholder value or primarily channels value to founder private benefits.').

omega_variable(
    capital_constraint_solved_or_persistent,
    'Is the founding problem (founders cannot raise capital without ceding control) still live, or is it substantially solved such that dual-class now serves only founder rent-seeking?',
    'Counterfactual: founders who could raise public capital at equivalent terms (debt, secondary equity, private equity) without dual-class structures — do they choose dual-class or not? Historical analysis of whether capital was genuinely scarce at founding or control concentration was the founder''s preference.',
    'If founding problem is dead: mandatrophy is confirmed and the constraint''s persistence becomes pure inertia plus beneficiary control. If founding problem is live: the constraint retains a genuine coordination justification and the extraction is a cost of solving the coordination problem, not pure rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_constraint_solved_or_persistent, empirical, 'Whether dual-class voting persists because it solves a genuine capital problem or because founders prefer rent extraction.').

omega_variable(
    governance_alternative_feasibility,
    'Could minority shareholders'' governance interests be protected without abolishing dual-class voting entirely — through protective provisions, board composition mandates, or consent requirements for major decisions?',
    'Comparative analysis of alternative governance structures: private company protective provisions, European codetermination, Japanese main-bank monitoring, founder-led partnerships. Feasibility analysis of retrofitting protective provisions to public dual-class structures without founder resistance.',
    'If protective provisions are feasible: the constraint is a choice by founders to capture maximum governance value; if they are infeasible or founder-resistant: the constraint represents a structural equilibrium where founder veto prevents any modification. This affects whether the remedy is regulatory vs. market-driven.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(governance_alternative_feasibility, empirical, 'Whether intermediate governance structures can balance founder autonomy and minority protection.').

omega_variable(
    reading_foreclosure_stewardship_vs_extraction,
    'Are the founder_stewardship and minority_extraction readings logically compatible within a single framework, or does adopting one reading logically foreclose the other?',
    'Logical analysis: the stewardship reading asserts founder control produces value; the extraction reading asserts it primarily benefits the founder at shareholder expense. These can coexist if stewardship reading concedes some private benefits exist but net creation exceeds them, OR they foreclose if the definitions of ''shareholder value'' and ''private benefits'' are mutually exclusive.',
    'If compatible (coexist_with relation): both readings remain live and the disagreement is empirical (does stewardship outweigh extraction). If they foreclose: adoption of one reading logically requires rejecting the other''s core premise, requiring a reading_relations: forecloses assignment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_stewardship_vs_extraction, conceptual, 'Logical structure of the stewardship vs. extraction dichotomy in readings of dual-class legitimacy.').

omega_variable(
    disclosure_consent_vs_extraction_decoupling,
    'Does the disclosure_consent reading''s claim (legitimacy rests on informed consent under Securities Act disclosure) decouple entirely from the extraction assessment, or does it implicitly concede extraction by routing legitimacy through consent rather than control parity?',
    'Textual analysis of Securities Act disclosure rules and what they require disclosed about dual-class control. Empirical analysis: do Class B purchasers actually understand the governance implications, and does that understanding change their demand/price expectations?',
    'If decoupled: disclosure_consent is genuinely a separate reading grounding legitimacy in a different axis (consent vs. control parity), and all three readings coexist. If consent implicitly concedes extraction: disclosure_consent acknowledges the extraction and attempts to legitimize it through informed acceptance, which is a different kind of coexistence (extraction is real but legitimized by consent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disclosure_consent_vs_extraction_decoupling, conceptual, 'Whether disclosure_consent is a separate reading or a consent-based framing of the same extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__minority_extraction, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__minority_extraction, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(dual_tr_t0, observed).
narrative_ontology:measurement(dual_tr_t3, dual_class_legitimacy__minority_extraction, theater_ratio, 3, 0.31).
narrative_ontology:measurement_basis(dual_tr_t3, observed).
narrative_ontology:measurement(dual_tr_t6, dual_class_legitimacy__minority_extraction, theater_ratio, 6, 0.34).
narrative_ontology:measurement_basis(dual_tr_t6, observed).
narrative_ontology:measurement(dual_tr_t10, dual_class_legitimacy__minority_extraction, theater_ratio, 10, 0.37).
narrative_ontology:measurement_basis(dual_tr_t10, observed).
narrative_ontology:measurement(dual_tr_t15, dual_class_legitimacy__minority_extraction, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(dual_tr_t15, observed).
narrative_ontology:measurement(dual_tr_t20, dual_class_legitimacy__minority_extraction, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(dual_tr_t20, observed).
narrative_ontology:measurement(dual_tr_t25, dual_class_legitimacy__minority_extraction, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(dual_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__minority_extraction, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(dual_be_t0, observed).
narrative_ontology:measurement(dual_be_t3, dual_class_legitimacy__minority_extraction, base_extractiveness, 3, 0.62).
narrative_ontology:measurement_basis(dual_be_t3, observed).
narrative_ontology:measurement(dual_be_t6, dual_class_legitimacy__minority_extraction, base_extractiveness, 6, 0.66).
narrative_ontology:measurement_basis(dual_be_t6, observed).
narrative_ontology:measurement(dual_be_t10, dual_class_legitimacy__minority_extraction, base_extractiveness, 10, 0.71).
narrative_ontology:measurement_basis(dual_be_t10, observed).
narrative_ontology:measurement(dual_be_t15, dual_class_legitimacy__minority_extraction, base_extractiveness, 15, 0.76).
narrative_ontology:measurement_basis(dual_be_t15, observed).
narrative_ontology:measurement(dual_be_t20, dual_class_legitimacy__minority_extraction, base_extractiveness, 20, 0.78).
narrative_ontology:measurement_basis(dual_be_t20, observed).
narrative_ontology:measurement(dual_be_t25, dual_class_legitimacy__minority_extraction, base_extractiveness, 25, 0.79).
narrative_ontology:measurement_basis(dual_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__minority_extraction, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(dual_su_t0, observed).
narrative_ontology:measurement(dual_su_t3, dual_class_legitimacy__minority_extraction, suppression_requirement, 3, 0.58).
narrative_ontology:measurement_basis(dual_su_t3, observed).
narrative_ontology:measurement(dual_su_t6, dual_class_legitimacy__minority_extraction, suppression_requirement, 6, 0.62).
narrative_ontology:measurement_basis(dual_su_t6, observed).
narrative_ontology:measurement(dual_su_t10, dual_class_legitimacy__minority_extraction, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(dual_su_t10, observed).
narrative_ontology:measurement(dual_su_t15, dual_class_legitimacy__minority_extraction, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(dual_su_t15, observed).
narrative_ontology:measurement(dual_su_t20, dual_class_legitimacy__minority_extraction, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(dual_su_t20, observed).
narrative_ontology:measurement(dual_su_t25, dual_class_legitimacy__minority_extraction, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(dual_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__minority_extraction, identity_coordination).
narrative_ontology:boltzmann_floor_override(dual_class_legitimacy__minority_extraction, 0.12).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the dual_class_legitimacy kernel. The kernel is the contested legitimacy claim: are dual-class voting structures legitimate? This reading (minority_extraction) assesses the standing arrangement as extractive — governance value is transferred from public shareholders to founders despite public risk bearing and capital stake. Sibling readings offer alternative frames: founder_stewardship (founder control creates long-term value) and disclosure_consent (legitimacy rests on informed consent, not control parity). Each reading has its own ε, beneficiary/victim structure, and stakeholder configuration. They are linked via network.affects_constraints to enable cross-reading analysis and constraint-family consistency checks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dual_class_legitimacy__minority_extraction, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
