% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__minority_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   human_readable: Dual-Class Governance Value Transfer (Minority-Extraction Reading)
 *   domain: economic/legal/corporate_governance
 *
 * SUMMARY:
 *   Dual-class capital structures split equity into supervoting founder
 *   shares and one-vote public shares, and controlled-company exemptions at
 *   the listing venues waive independent-majority-board,
 *   compensation-committee, and nominating-committee requirements wherever
 *   the founder block holds voting majorities. This story instantiates the
 *   minority_extraction reading of the dual_class_legitimacy kernel: on this
 *   reading the standing arrangement under contest - perpetual supervoting
 *   charters plus the exemption regime shielding them - transfers governance
 *   value from public purchasers to the controlling block while Class A
 *   holders bear undiluted economic risk. Epsilon's referent is that standing
 *   arrangement as this reading assesses it, never the stewardship
 *   arrangement the founder_stewardship sibling would endorse. The claimed
 *   type and the metrics are authored independently: the claim states what
 *   this reading takes the structure to be; the metrics state what its
 *   operation looks like descriptively. KEY AGENTS (by structural
 *   relationship): - controlling_founders: primary beneficiary and agenda
 *   setter (institutional/arbitrage) - retains multi-vote control, sells down
 *   economics, sets board slate and pay - public_class_a_shareholders:
 *   primary target (powerless/constrained) - full capital risk,
 *   arithmetically nullified in contested votes - index_fund_managers:
 *   trapped intermediary payer (organized/trapped) - mandate-bound holding,
 *   votes under issuer relationships - employee_class_a_grantees: secondary
 *   target (moderate/identity_locked) - human-capital and equity risk fused
 *   to employment - exchange_listing_authorities: enforcement administrator
 *   and fee beneficiary (institutional/mobile) - writes and administers the
 *   controlled-company exemption - early_supervoting_investors: incidental
 *   beneficiary (powerful/mobile) - pre-offering supervoting holders with
 *   liquidated or liquidatable positions - proxy_advisors_igp_coalition:
 *   analytical observer-resistor (organized/analytical) - recommendations,
 *   campaigns, index-policy pressure - securities_regulator: observer
 *   (institutional/analytical) - disclosure-only posture since the vacated
 *   one-share-one-vote rule - shareholder_activists: excluded actor
 *   (organized/mobile) - business model foreclosed at dual-class issuers
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__minority_extraction, 0.71).
domain_priors:suppression_score(dual_class_legitimacy__minority_extraction, 0.62).
domain_priors:theater_ratio(dual_class_legitimacy__minority_extraction, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, extractiveness, 0.71).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__minority_extraction, tangled_rope).
narrative_ontology:human_readable(dual_class_legitimacy__minority_extraction, "Dual-Class Governance Value Transfer (Minority-Extraction Reading)").
narrative_ontology:topic_domain(dual_class_legitimacy__minority_extraction, "economic/legal/corporate_governance").

domain_priors:requires_active_enforcement(dual_class_legitimacy__minority_extraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__minority_extraction, '1ee56b4b-d7a7-42a7-bc77-d483e91ab121').
narrative_ontology:cs_kernel_codification('1ee56b4b-d7a7-42a7-bc77-d483e91ab121', formalized).
narrative_ontology:cs_authority_grounding('1ee56b4b-d7a7-42a7-bc77-d483e91ab121', lineage).
narrative_ontology:cs_interpretation_layer_present('1ee56b4b-d7a7-42a7-bc77-d483e91ab121').
narrative_ontology:cs_reading_relation('1ee56b4b-d7a7-42a7-bc77-d483e91ab121', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('1ee56b4b-d7a7-42a7-bc77-d483e91ab121', dual_class_legitimacy__disclosure_consent, coexists_with).
narrative_ontology:cs_axiom('1ee56b4b-d7a7-42a7-bc77-d483e91ab121', foundational, voice_entitlement_tracks_capital_and_risk).
narrative_ontology:cs_axiom_status(voice_entitlement_tracks_capital_and_risk, holdable).
narrative_ontology:cs_axiom_grounding('1ee56b4b-d7a7-42a7-bc77-d483e91ab121', voice_entitlement_tracks_capital_and_risk, deontological).
narrative_ontology:cs_axiom('1ee56b4b-d7a7-42a7-bc77-d483e91ab121', secondary, ex_ante_disclosure_does_not_waive_parity_entitlement).
narrative_ontology:cs_axiom_status(ex_ante_disclosure_does_not_waive_parity_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('1ee56b4b-d7a7-42a7-bc77-d483e91ab121', ex_ante_disclosure_does_not_waive_parity_entitlement, conventional).
narrative_ontology:cs_reference_frame('1ee56b4b-d7a7-42a7-bc77-d483e91ab121', capital_risk_voice_parity).
narrative_ontology:cs_drift_state('1ee56b4b-d7a7-42a7-bc77-d483e91ab121', contemporary_dual_class_proliferation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1ee56b4b-d7a7-42a7-bc77-d483e91ab121', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__minority_extraction, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, controlling_founders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, early_supervoting_investors).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, public_class_a_shareholders).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, index_fund_managers).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, employee_class_a_grantees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, exchange_listing_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold supervoting shares carrying multiple votes per share, typically ten to one, retained after the public offering. Set the board slate, approve or block mergers and charter amendments, and determine executive compensation and succession. Can diversify personal wealth by selling economic shares into the market while retaining voting control, and can place shares in family trusts across generations. Losing control would require either selling votes or a charter amendment they themselves must approve.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, controlling_founders, agenda_setter,
    institutional, generational, arbitrage, global).

% Venture investors, angels, and early employees who received supervoting stock before the offering. Their economic exposure is often partially exited through secondary sales at premium valuations reflecting the growth story, while their votes remain pooled behind the founder block. They bear little ongoing governance burden and face no barrier to liquidating remaining positions.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, early_supervoting_investors, beneficiary,
    powerful, biographical, mobile, global).

% Purchase one-vote shares at the offering price and bear full downside risk on their capital. Receive annual meeting notices, proxy ballots decisive only in uncontested matters, and say-on-pay votes the controlling block outnumbers. Exit consists of selling at whatever price the market assigns after discounting for the governance gap; there is no mechanism to renegotiate terms after purchase.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, public_class_a_shareholders, payer,
    powerless, biographical, constrained, global).

% Must hold the one-vote shares of eligible issuers because their funds track benchmarks; declining to buy means tracking error against the mandate. Vote the shares under policies that weigh ongoing issuer relationships, and engage management through private correspondence and votes against directors that cannot alter board composition while the founder block controls the outcome. Their exit option is functionally closed by the mandate itself.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, index_fund_managers, payer,
    organized, generational, trapped, global).

% Receive restricted stock and options settled in one-vote shares as a large fraction of compensation. Career progression, multi-year vesting schedules, and insider status tie them to the firm; selling vested shares early carries plan penalties and signaling costs. They bear firm-specific human-capital risk alongside equity risk with the same silent ballot as outside holders.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, employee_class_a_grantees, payer,
    moderate, biographical, identity_locked, national).

% Operate the listing venues and write the listing standards, including the controlled-company exemption that waives independent-majority-board, compensation-committee, and nominating-committee requirements for issuers where a person or group holds a majority of voting power. Collect listing fees that scale with issuer attractiveness and compete with other venues for marquee technology listings. Administer exemption determinations issuer by issuer.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, exchange_listing_authorities, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__minority_extraction, exchange_listing_authorities, agenda_setter).

% Publish voting recommendations and policy frameworks evaluating tiered-voting issuers, coordinate institutional investor positions through associations, and campaign for sunset clauses and index-exclusion policies. Their leverage runs through recommendation uptake and index-provider rule changes rather than through any vote they cast directly.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, proxy_advisors_igp_coalition, observer,
    organized, biographical, analytical, global).

% Writes disclosure requirements for tiered-voting offerings and periodically studies the structure. A prior attempt to mandate one-share-one-vote for exchange-listed issuers was vacated by the reviewing court in the late 1980s, and the agency has since confined itself to disclosure and study, acting only on fraud or disclosure failures.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, securities_regulator, observer,
    institutional, generational, analytical, national).

% Specialist funds and coalitions that seek board seats, governance reforms, or sales at controlled companies. The vote differential makes director contests and merger votes arithmetically unwinnable at tiered-voting issuers, so they concentrate on single-class targets; their business model is excluded from this population by the structure itself.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, shareholder_activists, excluded,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__minority_extraction, controlling_founders).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__minority_extraction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrated founder control solves a real coordination problem: it provides unified, fast-moving strategic authority during scaling, insulates long-horizon research and mission investments from takeover arbitrage and activist cost-cutting, and gives capital markets a single accountable decision-maker at firms where dispersed ownership would produce paralysis or myopia.
% TRANSFER_FUNCTION: Moves voting control, board-composition power, and merger-approval rights from public one-vote purchasers to the founder block at zero marginal price, while moving capital from public purchasers into the company at par. Moves the private benefits of control - compensation setting, related-party transactions, succession selection, sale-process steering - toward the controlling block, funded by risk borne disproportionately by those without corresponding voice.
% ABSENT_VOICES: Future minority purchasers at the offering, who encounter the terms take-it-or-leave-it and cannot negotiate them; index-fund beneficial owners - retirement savers - whose proxies are voted by asset managers facing issuer-business conflicts; employees whose compensation equity carries the same silent terms; and rival bidders whose offers the vote differential forecloses before they are ever made.
% DISAPPEARANCE_RATIONALE: If tiered-voting charters and the controlled-company exemption vanished overnight, offering structures, index-eligibility rules, founder wealth-diversification paths, and the market for corporate control would all reorganize: issuers would either accept one-share-one-vote and price takeover vulnerability into strategy, or stay private longer; index providers would re-admit newly single-class issuers; activist capital would expand its addressable universe; and governance discounts would compress as voting rights repriced.
% FOUNDING_PROBLEM: Founders raising public capital feared losing control of the enterprise they built: hostile tender offers, activist campaigns demanding cost cuts and divestitures, and quarterly-earnings pressure disrupting long-horizon investment. Tiered voting was built to let founders sell equity without selling control.
% FOUNDING_PROBLEM_CORROBORATION: No single attestation exists; the record is split along non-beneficiary lines. Academic empirical work on control premia, minority discounts, and post-listing performance - much of it by scholars with no position in the arrangement - corroborates that the anti-takeover rationale overlaps substantially with entrenchment effects, and institutional investor associations and proxy advisors attest the protective function is overstated. Founder-aligned counsel and the issuers themselves attest the threats remain live, citing recurring activist campaigns at comparable single-class firms. Corroboration for the contested status therefore comes from outside the benefiting parties on both sides, which is itself the signal that the founding problem's liveness is genuinely disputed rather than settled.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__minority_extraction, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__minority_extraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__minority_extraction, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dual_class_legitimacy__minority_extraction, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__minority_extraction, 0.71, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored high (0.71 at interval end) because the vote differential prices governance value away from Class A purchasers at zero marginal charge: control-block premia documented in the voting-rights literature, minority governance discounts, and compensation-setting power concentrated in the founder's own nominees all register as transfer from risk-bearers to the controlling block. Suppression (0.62) is structural, not internalized: proxy contests are arithmetically futile under 10:1 votes, charter amendment thresholds run through the incumbent block, the controlled-company exemption removes the independent-committee checks precisely where control is most concentrated, and the federal one-share-one-vote attempt was vacated in the late 1980s, leaving no external forum. It is not higher because exit-by-sale remains open to non-index holders and disclosure is robust. Theater (0.46) reflects governance ritual whose outputs are predetermined: annual meetings, say-on-pay votes, and formally independent committees operate as real procedures producing controlled outcomes. Accessibility_collapse (0.55) is moderate: once the structure is understood, alternatives exist (single-class issuers, exclusion screens, engagement), but they collapse entirely for mandate-bound index holders. Resistance (0.60) is real and organized - adverse proxy-advisor recommendations, index exclusion policies for new dual-class listings, association campaigns, legislative hearings, and voluntary collapses under investor pressure - yet it has slowed rather than reversed adoption, which is why the extractiveness series rises monotonically. The measurement grid maps roughly onto the modern dual-class era (takeover-wave origins at t0, mainstreaming after the 2004 search-engine listing near t18, exemption codification near t24, the zero-vote extension near t30, and the policy backlash near t36); all three tracked metrics are authored at every shared time point on one grid. The claim (tangled_rope) and the metrics are independent authored facts: the engine computes per-seat types from the structural data, and any divergence between the claim and a computed seat type is the datum, not an error to reconcile.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the controlling_founders seat the arrangement is a coordination device they built and personally maintain: unified leadership, protected horizons, insulation from takeover arbitrage. From the public_class_a_shareholders and employee_class_a_grantees seats the same charter operates as paying full price for equity while receiving a discounted claim on its governance. Same-level lateral dynamics matter: public_class_a_shareholders and index_fund_managers hold the identical security at the same nominal tier, yet differ sharply in exit - the former can sell and realize the governance discount, the latter is mandate-bound and cannot exit without abandoning the fund's purpose, so the same constraint binds them with different force. Inter-institutionally, the exchange_listing_authorities administer the exemption regime while collecting fees that scale with attractive listings, whereas the securities_regulator stands observationally apart after losing its rulemaking attempt; the two institutional observers experience the same structure as revenue and as jurisdictional limit respectively.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: controlling_founders (agenda_setter plus beneficiary, arbitrage-grade exit via selling economics while retaining votes) sits nearest the beneficiary pole; early_supervoting_investors (beneficiary, mobile exit) sits close behind. Victim declarations drive the opposite pole: public_class_a_shareholders (victim, constrained exit - selling crystallizes the discount the structure creates) and employee_class_a_grantees (victim, identity_locked exit - unvested equity, career path, and insider status fuse the holder to the firm) sit near the full-target end, with index_fund_managers (victim, trapped by mandate) at the extreme target end despite their organized power, because power without exit does not dampen extraction. Exchange_listing_authorities derive low directionality from their beneficiary role, which is accurate: they collect listing fees and administer exemptions without bearing the governance discount. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and by the arrangement's global scope, which raises verification difficulty for dispersed holders across jurisdictions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - protecting founder-led long-horizon strategy from hostile takeovers, activist cost-cutting, and quarterly myopia while raising public capital - is contested rather than dead: founders and founder-aligned counsel attest it remains live, while investor-side scholarship and institutional investor bodies attest the protective function is substantially overlapped by entrenchment. Because the status is contested rather than dead, mandatrophy_resolved is deliberately not declared. The tangled_rope claim is what prevents misclassification in both directions: calling the arrangement a snare would erase the genuine coordination function (someone must govern, and concentrated control demonstrably solves takeover and horizon problems for some issuers); calling it a rope would erase the asymmetric transfer (the same structure that coordinates also moves governance value from risk-bearers to the controlling block at no charge). The R5 mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges and finds no zombie signature: the arrangement persists because a disputed-but-alive protective function and an uncontested rent stream are fused in one charter. The sunset_synthesis_feasibility omega marks the migration path: if time-based sunsets prove dominant, the arrangement's steady-state justification collapses into transition and the classification should migrate toward scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the minority_extraction reading of the dual_class_legitimacy kernel; would the founder_stewardship or disclosure_consent sibling readings classify the identical standing arrangement differently?',
    'Generate the sibling-reading stories and compare authored epsilon and computed types over the same referent (perpetual supervoting charters plus controlled-company exemptions); divergence localizes the disagreement to the legitimacy premise rather than to the facts.',
    'Under founder_stewardship, epsilon drops toward coordination cost and the computed type trends rope; under disclosure_consent, informed consent partially legitimates the terms and epsilon falls moderately. Only this reading authors the full governance-value-transfer epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame omega: reading-indexed epsilon over a shared kernel and shared referent.').

omega_variable(
    private_benefits_of_control_magnitude,
    'How large are the private benefits of control actually captured by the controlling block (compensation premiums, related-party transactions, succession rents, sale-process steering) relative to any public-goods stewardship value the concentration produces?',
    'Event studies around dual-class sunset proposals and voluntary collapses; cross-sectional comparison of executive compensation and related-party transaction incidence between dual-class issuers and matched single-class controls.',
    'Large measured private benefits confirm the transfer reading and push effective extraction higher; negligible private benefits would support the stewardship sibling and shrink epsilon toward the coordination floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_benefits_of_control_magnitude, empirical, 'Magnitude of capturable private benefits versus stewardship public goods.').

omega_variable(
    rational_apathy_counterfactual,
    'Would dispersed Class A holders actually convert restored votes into monitoring, or does rational apathy mean the transferred governance value is partly hypothetical and therefore partly unexperienced?',
    'Natural experiments where votes were restored (sunset conversions, collapses at founder exit or death): measure subsequent activism incidence, proxy contest rates, and valuation changes.',
    'If restored votes go unused, part of the claimed transfer is not experienced loss and epsilon should be revised downward; if restored votes activate monitoring and valuations respond, the transfer reading is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rational_apathy_counterfactual, empirical, 'Whether the transferred governance value would be exercised if returned.').

omega_variable(
    sunset_synthesis_feasibility,
    'Would time-based sunset clauses (supervoting decaying to one-share-one-vote after a fixed horizon) preserve the coordination benefit while eliminating the perpetual transfer, and is that hybrid a third reading rather than a resolution of this one?',
    'Compare firm outcomes and minority-holder valuations across issuers with adopted sunsets, perpetual dual-class issuers, and single-class controls.',
    'If sunsets dominate, the arrangement is better modeled as transitional support and the classification migrates toward scaffold; if sunsets fail or are resisted indefinitely, the perpetual transfer stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_synthesis_feasibility, preference, 'Whether a sunset hybrid dissolves the coordination-extraction tangle.').

omega_variable(
    consent_under_take_it_or_leave_it_terms,
    'Does ex ante disclosure-based consent at the offering neutralize the transfer claim, or is consent structurally hollow when terms are non-negotiable at issuance and benchmark inclusion later removes even the exit option?',
    'Conceptual analysis of the consent conditions plus behavioral evidence on purchaser comprehension of dual-class risk factors; observe whether persistent governance-discount pricing reveals consent given under uninformed or coerced conditions.',
    'If consent is hollow, the disclosure_consent sibling loses its legitimating force and this reading''s epsilon stands intact; if consent is robust, part of the measured transfer converts into a priced-in trade rather than imposed loss.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_under_take_it_or_leave_it_terms, conceptual, 'Validity of disclosure-based consent as a waiver of the parity entitlement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__minority_extraction, 0, 42).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_class_minority_extraction_tr_t0, dual_class_legitimacy__minority_extraction, theater_ratio, 0, 0.26).
narrative_ontology:measurement(dual_class_minority_extraction_tr_t6, dual_class_legitimacy__minority_extraction, theater_ratio, 6, 0.29).
narrative_ontology:measurement(dual_class_minority_extraction_tr_t12, dual_class_legitimacy__minority_extraction, theater_ratio, 12, 0.32).
narrative_ontology:measurement(dual_class_minority_extraction_tr_t18, dual_class_legitimacy__minority_extraction, theater_ratio, 18, 0.35).
narrative_ontology:measurement(dual_class_minority_extraction_tr_t24, dual_class_legitimacy__minority_extraction, theater_ratio, 24, 0.38).
narrative_ontology:measurement(dual_class_minority_extraction_tr_t30, dual_class_legitimacy__minority_extraction, theater_ratio, 30, 0.41).
narrative_ontology:measurement(dual_class_minority_extraction_tr_t36, dual_class_legitimacy__minority_extraction, theater_ratio, 36, 0.44).
narrative_ontology:measurement(dual_class_minority_extraction_tr_t42, dual_class_legitimacy__minority_extraction, theater_ratio, 42, 0.46).

% Extraction over time
narrative_ontology:measurement(dual_class_minority_extraction_be_t0, dual_class_legitimacy__minority_extraction, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(dual_class_minority_extraction_be_t6, dual_class_legitimacy__minority_extraction, base_extractiveness, 6, 0.54).
narrative_ontology:measurement(dual_class_minority_extraction_be_t12, dual_class_legitimacy__minority_extraction, base_extractiveness, 12, 0.59).
narrative_ontology:measurement(dual_class_minority_extraction_be_t18, dual_class_legitimacy__minority_extraction, base_extractiveness, 18, 0.63).
narrative_ontology:measurement(dual_class_minority_extraction_be_t24, dual_class_legitimacy__minority_extraction, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(dual_class_minority_extraction_be_t30, dual_class_legitimacy__minority_extraction, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(dual_class_minority_extraction_be_t36, dual_class_legitimacy__minority_extraction, base_extractiveness, 36, 0.7).
narrative_ontology:measurement(dual_class_minority_extraction_be_t42, dual_class_legitimacy__minority_extraction, base_extractiveness, 42, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(dual_class_minority_extraction_su_t0, dual_class_legitimacy__minority_extraction, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(dual_class_minority_extraction_su_t6, dual_class_legitimacy__minority_extraction, suppression_requirement, 6, 0.52).
narrative_ontology:measurement(dual_class_minority_extraction_su_t12, dual_class_legitimacy__minority_extraction, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(dual_class_minority_extraction_su_t18, dual_class_legitimacy__minority_extraction, suppression_requirement, 18, 0.57).
narrative_ontology:measurement(dual_class_minority_extraction_su_t24, dual_class_legitimacy__minority_extraction, suppression_requirement, 24, 0.59).
narrative_ontology:measurement(dual_class_minority_extraction_su_t30, dual_class_legitimacy__minority_extraction, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(dual_class_minority_extraction_su_t36, dual_class_legitimacy__minority_extraction, suppression_requirement, 36, 0.61).
narrative_ontology:measurement(dual_class_minority_extraction_su_t42, dual_class_legitimacy__minority_extraction, suppression_requirement, 42, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__minority_extraction, resource_allocation).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'dual-class legitimacy'. The natural-language concept covers three structurally distinct legitimacy claims that share one referent (tiered voting charters plus controlled-company exemptions) and diverge on epsilon: founder_stewardship authors low epsilon (control as service), disclosure_consent authors low-to-moderate epsilon (control as consented trade), and this story, minority_extraction, authors high epsilon (control as uncompensated transfer). Per the epsilon-invariance principle these are separate stories linked by network edges rather than one story with a legitimacy parameter. The upstream sibling (founder_stewardship) supplies the justification cited in charters and listing petitions; this reading supplies the critique that drives disclosure enhancement and sunset proposals, which changes the downstream sibling's operating environment without resolving the dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
