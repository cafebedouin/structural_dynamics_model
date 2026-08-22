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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: dual_class_legitimacy__minority_extraction
 *   human_readable: Dual-Class Share Structure — Minority Extraction Reading
 *   domain: Corporate Governance / Securities Law / Organizational Economics
 *
 * SUMMARY:
 *   This story instantiates the minority_extraction reading of the
 *   dual_class_legitimacy kernel: the standing arrangement is dual-class
 *   share structures (and the controlled-company exemptions that ride
 *   alongside them) as they exist in current public markets, assessed from
 *   the position that governance should track capital and risk. Under this
 *   reading, the founder-stewardship justification is read as cover for an
 *   entrenchment mechanism that becomes more extractive the longer it
 *   persists without a sunset, because the original long-horizon
 *   justification weakens over decades while the control premium the founder
 *   extracts (through pay, related-party deals, and unreviewable strategic
 *   choices) compounds. This is NOT a story about whether disclosure was
 *   adequate at IPO (that is the disclosure_consent reading) nor about
 *   whether founder control produces good outcomes for the company (that is
 *   the founder_stewardship reading) — it is a story about whether governance
 *   proportional to capital-at-risk is the correct baseline against which the
 *   arrangement should be measured, and under that baseline the arrangement
 *   is substantially extractive.
 *
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
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__minority_extraction, tangled_rope).
narrative_ontology:human_readable(dual_class_legitimacy__minority_extraction, "Dual-Class Share Structure — Minority Extraction Reading").
narrative_ontology:topic_domain(dual_class_legitimacy__minority_extraction, "Corporate Governance / Securities Law / Organizational Economics").

domain_priors:requires_active_enforcement(dual_class_legitimacy__minority_extraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__minority_extraction, '33aa6b1e-6a8a-46ad-8573-a5b52498fa0e').
narrative_ontology:cs_kernel_codification('33aa6b1e-6a8a-46ad-8573-a5b52498fa0e', formalized).
narrative_ontology:cs_authority_grounding('33aa6b1e-6a8a-46ad-8573-a5b52498fa0e', extraction).
narrative_ontology:cs_interpretation_layer_present('33aa6b1e-6a8a-46ad-8573-a5b52498fa0e').
narrative_ontology:cs_reading_relation('33aa6b1e-6a8a-46ad-8573-a5b52498fa0e', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('33aa6b1e-6a8a-46ad-8573-a5b52498fa0e', dual_class_legitimacy__disclosure_consent, influences).
narrative_ontology:cs_axiom('33aa6b1e-6a8a-46ad-8573-a5b52498fa0e', foundational, voting_power_must_track_capital_at_risk).
narrative_ontology:cs_axiom_status(voting_power_must_track_capital_at_risk, holdable).
narrative_ontology:cs_axiom_grounding('33aa6b1e-6a8a-46ad-8573-a5b52498fa0e', voting_power_must_track_capital_at_risk, deontological).
narrative_ontology:cs_axiom('33aa6b1e-6a8a-46ad-8573-a5b52498fa0e', secondary, perpetual_uncoupled_control_is_illegitimate_absent_sunset).
narrative_ontology:cs_axiom_status(perpetual_uncoupled_control_is_illegitimate_absent_sunset, holdable).
narrative_ontology:cs_axiom_grounding('33aa6b1e-6a8a-46ad-8573-a5b52498fa0e', perpetual_uncoupled_control_is_illegitimate_absent_sunset, instrumental).
narrative_ontology:cs_reference_frame('33aa6b1e-6a8a-46ad-8573-a5b52498fa0e', capital_proportional_voting_norm).
narrative_ontology:cs_drift_state('33aa6b1e-6a8a-46ad-8573-a5b52498fa0e', contemporary_tech_ipo_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('33aa6b1e-6a8a-46ad-8573-a5b52498fa0e', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__minority_extraction, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, founder_control_block).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, class_a_public_shareholders).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, index_fund_beneficial_owners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, stock_exchanges_and_listing_committees).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__minority_extraction, one_share_one_vote_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds Class B or founder shares carrying supervoting rights (typically 10:1 or higher), controlling board composition, M&A decisions, and charter amendments while holding a minority of the economic capital at risk. Sets governance terms at IPO and can invoke controlled-company exemptions from stock-exchange independence requirements. Faces essentially no exit pressure — control is entrenched by the share structure itself, not by continued performance.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, founder_control_block, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__minority_extraction, founder_control_block, beneficiary).

% Purchase common stock carrying one vote (or often zero effective votes given the control block's supermajority) while bearing full pro-rata economic risk of losses, dilution, and self-dealing transactions. Can sell shares but cannot exit the governance arrangement without exiting the investment itself; voting on director slates, executive pay, and related-party transactions is structurally decorative once the founder holds a voting majority.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, class_a_public_shareholders, payer,
    powerless, biographical, constrained, national).

% Retail savers and pensioners whose capital sits in index funds that must hold the dual-class stock to track a benchmark, regardless of governance terms. They have no individual voice in proxy votes (delegated to fund managers) and no practical ability to divest a single name from a diversified index product without exiting index investing altogether — a form of structural lock-in one step removed from direct ownership.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, index_fund_beneficial_owners, payer,
    moderate, generational, trapped, national).

% Large asset managers vote proxies and occasionally publish policies opposing dual-class listings, but their objections are advisory — they cannot compel a sunset or recapitalization once the structure is in the charter, and their own fiduciary mandate to track indices limits their ability to simply refuse to hold the stock. Their formal objections at IPO are routinely overridden by underwriter and issuer leverage over listing terms.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, institutional_asset_managers, excluded,
    organized, biographical, constrained, national).

% Compete with each other for high-profile listings and have progressively loosened voting-structure requirements (including controlled-company exemptions from independent-board and compensation-committee rules) to attract founder-controlled companies rather than lose the listing to a rival exchange or an alternative jurisdiction. They collect listing fees and prestige from marquee dual-class IPOs.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, stock_exchanges_and_listing_committees, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__minority_extraction, stock_exchanges_and_listing_committees, beneficiary).

% Oversee disclosure adequacy but have historically declined to mandate voting-rights parity or automatic sunset provisions, treating governance structure as a matter for exchange listing rules and shareholder choice at purchase rather than a matter of securities fraud. Their restraint is itself part of what allows the structure to persist unmodified.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, securities_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__minority_extraction, founder_control_block).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__minority_extraction, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Dual-class structures genuinely solve a founder-horizon problem: they let a founding team execute long-term strategy without being forced into short-term capital-market discipline by quarterly activist pressure, in principle protecting all shareholders from myopic value destruction.
% TRANSFER_FUNCTION: Moves governance control — the ability to set strategy, approve related-party transactions, set executive pay, and block takeovers — from the holders of the majority of economic capital (Class A public shareholders) to the holders of a minority of capital who retain supervoting shares (the founder control block), while risk of loss remains proportional to capital held, not to votes held.
% ABSENT_VOICES: Class A shareholders as a class have no seat at the table when the dual-class structure is set at IPO — the terms are fixed before the public tranche is even offered, so the 'consent' the disclosure_consent reading relies on is consent to a structure already foreclosed, not a negotiated term. Retail beneficial owners inside index funds have no voice in the underlying proxy vote at all.
% DISAPPEARANCE_RATIONALE: If dual-class structures and controlled-company exemptions disappeared overnight, voting power would immediately reconvert to be proportional to capital at risk; boards currently insulated from removal would become contestable, self-dealing related-party transactions would face independent committee review for the first time, and founder control blocks would need to acquire additional capital or build coalition support to retain strategic authority — a substantial reallocation of governance value toward the class that bears the pro-rata financial risk.
% FOUNDING_PROBLEM: Founders taking companies public wanted to prevent hostile takeover or short-term activist pressure from derailing long-horizon strategy shortly after IPO, when the company is most vulnerable to opportunistic capital-market discipline.
% FOUNDING_PROBLEM_CORROBORATION: Founders and their underwriters attest the problem remains live indefinitely, citing activist-investor pressure years or decades post-IPO. Independent corporate-governance researchers (e.g., studies cited in SEC comment letters and by the Council of Institutional Investors) and several major index providers (which have restricted new dual-class listings from certain indices) attest from outside the founder-benefiting class that the structural need, if real at IPO, does not justify perpetual control absent a sunset — supporting the contested status rather than a settled 'live' verdict.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__minority_extraction, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__minority_extraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__minority_extraction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction rises from 0.45 to 0.71 over the interval because dual-class structures without sunsets compound: the initial control premium at IPO is modest relative to the growing gap between economic ownership and voting power as founders sell down economic stakes (reducing capital at risk) while retaining supervoting shares (retaining full control) — a pattern documented at several marquee dual-class companies. Suppression (0.68) reflects that Class A holders' only real remedy — refusing to invest — is foreclosed once the stock enters benchmark indices that passive capital must track. Theater ratio (0.42) reflects that proxy voting, shareholder proposals, and governance committees continue to operate visibly while structurally unable to change the outcome the control block does not want changed.
 *
 * DIRECTIONALITY LOGIC:
 *   The founder control block is the structural beneficiary: it collects governance value (control over pay, M&A, related-party transactions) disproportionate to its capital at risk, and its exit options are effectively arbitrage-grade (it can sell down economic exposure while retaining voting control, the opposite of most agents' risk-exit coupling). Class A public shareholders and index-fund beneficial owners are targets: they bear pro-rata risk without proportional voice, and their exit is constrained or trapped by benchmark-tracking obligations. Institutional asset managers sit in an excluded-advisory position — organized enough to object formally but structurally unable to compel change once terms are set.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting long-horizon strategy from short-term activist pressure) may have been genuinely live at IPO for some issuers. This reading holds that status as contested rather than dead outright, because the problem is not obviously permanent — the mandatrophy question is precisely whether a mechanism justified as transitional protection against IPO-era volatility has, absent any sunset clause, calcified into indefinite entrenchment. Classifying this as tangled_rope rather than snare preserves the coordination function (there IS a real problem dual-class structures address) while still naming the asymmetric extraction (public shareholders subsidize control they cannot exercise) that the coordination story provides cover for — a pure snare framing would erase the founder-stewardship reading's legitimate empirical basis; a pure rope framing would erase the measured, compounding cost to Class A holders.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sunset_clause_counterfactual,
    'Would a mandatory time-limited sunset (converting dual-class to single-class after a fixed term or triggering event) resolve the extraction without eliminating the coordination benefit the founder_stewardship reading identifies?',
    'Comparative analysis of dual-class companies with voluntary sunset provisions (e.g., time-based or founder-departure triggers) versus perpetual dual-class companies, measuring control-premium trajectory and shareholder-value outcomes post-sunset.',
    'If sunset-provision companies show convergence toward proportional governance without value destruction, this reading''s tangled_rope classification would strengthen (confirming a fixable coordination/extraction hybrid); if sunset triggers are routinely renegotiated or waived, the classification moves toward snare (confirming the coordination story is durable cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_counterfactual, empirical, 'Whether a sunset clause would separate the genuine coordination function from the extractive entrenchment.').

omega_variable(
    kernel_baseline_disagreement,
    'Is ''governance proportional to capital and risk borne'' the correct normative baseline for evaluating dual-class structures, or is disclosed voluntary consent at purchase the correct baseline (as the disclosure_consent reading holds), or is founder execution capacity the correct baseline (as the founder_stewardship reading holds)?',
    'This is not resolvable by further data — it is a genuine disagreement about which baseline securities law and corporate governance theory should adopt, contested among the three sibling readings of the dual_class_legitimacy kernel.',
    'The choice of baseline determines which reading''s ε is treated as authoritative for policy purposes; this story''s high ε (0.71) is a fact about THIS reading''s baseline, not a fact independent of baseline choice. Adopting a different baseline would not change the empirical facts but would change which facts are classified as extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_baseline_disagreement, preference, 'Which normative baseline governs legitimacy assessment of dual-class control — the core kernel contest.').

omega_variable(
    controlled_company_exemption_scope,
    'Does the stock-exchange controlled-company exemption (from independent board majority, independent compensation and nominating committees) meaningfully increase the extraction beyond what the voting structure alone would produce?',
    'Compare governance outcomes (executive pay ratios, related-party transaction frequency and terms, board turnover) between dual-class companies that voluntarily maintain independent committees despite qualifying for the exemption, versus those that use the full exemption.',
    'If the exemption itself drives incremental extraction beyond the voting structure, that supports treating the exemption as a separable extraction lever policymakers could remove without touching the voting structure itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(controlled_company_exemption_scope, empirical, 'Whether the exchange listing exemption is an independent extraction mechanism or merely downstream of the voting structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__minority_extraction, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__minority_extraction, theater_ratio, 0, 0.22).
narrative_ontology:measurement(dual_tr_t4, dual_class_legitimacy__minority_extraction, theater_ratio, 4, 0.28).
narrative_ontology:measurement(dual_tr_t8, dual_class_legitimacy__minority_extraction, theater_ratio, 8, 0.33).
narrative_ontology:measurement(dual_tr_t12, dual_class_legitimacy__minority_extraction, theater_ratio, 12, 0.37).
narrative_ontology:measurement(dual_tr_t16, dual_class_legitimacy__minority_extraction, theater_ratio, 16, 0.4).
narrative_ontology:measurement(dual_tr_t20, dual_class_legitimacy__minority_extraction, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__minority_extraction, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(dual_be_t4, dual_class_legitimacy__minority_extraction, base_extractiveness, 4, 0.52).
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

narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the dual_class_legitimacy kernel, each a separate story with its own ε and stakeholder structure per the ε-invariance principle. dual_class_legitimacy__founder_stewardship reads the same standing arrangement as legitimate long-horizon coordination (low ε). dual_class_legitimacy__disclosure_consent reads it as a disclosed, consented-to term that is not itself the locus of legitimacy analysis. This story (minority_extraction) reads it as substantial, compounding extraction against a capital-proportional-governance baseline (high ε, tangled_rope). The three are linked, not merged, because their ε values differ by construction of differing baselines, not by differing facts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
