% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__minority_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   human_readable: Dual-Class Governance Architecture Assessed from the Minority-Extraction Reading
 *   domain: economic/legal/corporate_governance
 *
 * SUMMARY:
 *   The standing arrangement under contest is the dual-class capital
 *   structure as it actually operates: a founding group holds super-voting
 *   shares (commonly ten or twenty votes per share) authored into the charter
 *   at offering, the public purchases the low-vote class at full economic
 *   price, and controlled-company exemptions relieve controlled issuers of
 *   independent-majority-board, independent-compensation-committee, and
 *   independent-nominating requirements. Persistence rests on actively
 *   maintained machinery — charter provisions, exchange grandfathering of
 *   legacy structures, exemption administration, and jurisdictional
 *   competition among listing venues. This file instantiates the
 *   minority_extraction reading of the kernel dual_class_legitimacy: minority
 *   holders are entitled to governance proportional to capital and risk
 *   borne, so the arrangement is assessed as a transfer of governance value
 *   from contributing public holders to the controlling bloc. Per the
 *   epsilon-referent rule, the referent is fixed (the standing arrangement);
 *   the epsilon value is reading-indexed — the sibling stories author their
 *   own values over the same referent. Claim/metric independence:
 *   claimed_type is asserted on structural grounds (a real coordination
 *   function, real asymmetric extraction, load-bearing enforcement); the
 *   metrics are authored as descriptive truth of the arrangement's operation,
 *   not tuned to the claim or to any predicted engine output.
 *
 * KEY AGENTS:
 *   - - dual_class_founder_insiders: agenda-setter and principal collector ([powerful]/[arbitrage]) — authors and defends the vote map, converts a small economic stake into decisive control
 *   - - entrenched_incumbent_boards: secondary beneficiary ([powerful]/[constrained]) — occupies offices held at the controlling bloc's pleasure
 *   - - exchange_listing_authorities: agenda-setter ([institutional]/[arbitrage]) — administers listing standards, exemptions, and grandfathering under competitive constraint
 *   - - ipo_underwriting_syndicates: beneficiary ([powerful]/[arbitrage]) — fees scale with founder-friendly structuring across repeat engagements
 *   - - institutional_class_a_managers: primary target ([institutional]/[trapped]) — bears full economic risk on fractionated voice under index and mandate lock
 *   - - retail_pension_participants: ultimate target ([powerless]/[trapped]) — absorbs governance losses through pooled vehicles with no direct voice
 *   - - proxy_advisory_firms: commercial analytical seat ([organized]/[mobile]) — recommendations turn ceremonial where votes are predetermined
 *   - - sec_regulators: analytical observer ([institutional]/[analytical]) — regulates disclosure and process, declines structural intervention
 *   - - future_class_a_purchasers: structurally excluded ([powerless]/[trapped]) — enters after the consent moment that froze the architecture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__minority_extraction, 0.72).
domain_priors:suppression_score(dual_class_legitimacy__minority_extraction, 0.67).
domain_priors:theater_ratio(dual_class_legitimacy__minority_extraction, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, extractiveness, 0.72).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__minority_extraction, tangled_rope).
narrative_ontology:human_readable(dual_class_legitimacy__minority_extraction, "Dual-Class Governance Architecture Assessed from the Minority-Extraction Reading").
narrative_ontology:topic_domain(dual_class_legitimacy__minority_extraction, "economic/legal/corporate_governance").

domain_priors:requires_active_enforcement(dual_class_legitimacy__minority_extraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__minority_extraction, 'd68398c9-45e9-4338-8725-7663bbdf61d4').
narrative_ontology:cs_kernel_codification('d68398c9-45e9-4338-8725-7663bbdf61d4', fixed_text).
narrative_ontology:cs_authority_grounding('d68398c9-45e9-4338-8725-7663bbdf61d4', extraction).
narrative_ontology:cs_interpretation_layer_present('d68398c9-45e9-4338-8725-7663bbdf61d4').
narrative_ontology:cs_reading_relation('d68398c9-45e9-4338-8725-7663bbdf61d4', dual_class_legitimacy__founder_stewardship, forecloses).
narrative_ontology:cs_reading_relation('d68398c9-45e9-4338-8725-7663bbdf61d4', dual_class_legitimacy__disclosure_consent, forecloses).
narrative_ontology:cs_axiom('d68398c9-45e9-4338-8725-7663bbdf61d4', foundational, proportional_governance_entitlement).
narrative_ontology:cs_axiom_status(proportional_governance_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('d68398c9-45e9-4338-8725-7663bbdf61d4', proportional_governance_entitlement, deontological).
narrative_ontology:cs_axiom('d68398c9-45e9-4338-8725-7663bbdf61d4', secondary, controlled_exemption_violates_entitlement).
narrative_ontology:cs_axiom_status(controlled_exemption_violates_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('d68398c9-45e9-4338-8725-7663bbdf61d4', controlled_exemption_violates_entitlement, conventional).
narrative_ontology:cs_reference_frame('d68398c9-45e9-4338-8725-7663bbdf61d4', proportional_governance_baseline).
narrative_ontology:cs_drift_state('d68398c9-45e9-4338-8725-7663bbdf61d4', contemporary_dual_class_normalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d68398c9-45e9-4338-8725-7663bbdf61d4', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__minority_extraction, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, dual_class_founder_insiders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, entrenched_incumbent_boards).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, ipo_underwriting_syndicates).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, institutional_class_a_managers).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, retail_pension_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold super-voting shares (commonly ten or twenty votes per share) written into the charter at offering. Author the vote allocation, board-classification, and conversion provisions and defend them against amendment proposals. Retain decisive authority over the director slate, compensation ratification, strategic transactions, and any sale of the company while holding an economic stake far smaller than their voting weight. Monetization paths include control-block sales at premiums, borrowing against voting stock, and staggered conversion windows timed to personal tenure.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, dual_class_founder_insiders, agenda_setter,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__minority_extraction, dual_class_founder_insiders, beneficiary).

% Occupy directorships and executive offices that depend on continued backing of the controlling bloc rather than on competitive elections. Compensation and committee assignments flow through structures the controlling holder effectively appoints. Leaving means forfeiting position, compensation, and standing; staying means administering a board whose composition the controlling holder can reset at will.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, entrenched_incumbent_boards, beneficiary,
    powerful, biographical, constrained, global).

% Write and administer the listing standards under which these structures trade: admitting super-voting and limited-voting classes at listing, exempting controlled companies from independent-board, independent-compensation, and independent-nominating requirements, and grandfathering legacy structures adopted before tightenings. Compete with counterpart venues in other jurisdictions for offerings, so tightening standards domestically tends to shift issuance abroad rather than change the architecture.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, exchange_listing_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Structure and distribute offerings in which retention of founder control is a design objective. Fees and league-table standing scale with completed deals, and advising on share-class architecture, sunset windows, and defensive charter terms aligns their work product with issuer preferences. Repeat relationships concentrate on the same sponsor groups across successive offerings.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, ipo_underwriting_syndicates, beneficiary,
    powerful, immediate, arbitrage, global).

% Hold the low-vote class on behalf of index-tracking and actively managed pools. Tracking mandates require continued ownership regardless of governance posture; selling means abandoning the mandate or accepting tracking error. Voting weight per dollar committed is fractionated by design, and engagement faces a counterparty whose votes they cannot materially affect. They also court the same corporations for retirement-plan mandates, which raises the cost of adversarial stewardship.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, institutional_class_a_managers, payer,
    institutional, biographical, trapped, global).

% Own the underlying capital through pooled retirement and mutual-fund vehicles, with allocations set by plan menus and fund lineups rather than personal choice. Governance losses arrive as marginally lower net returns; the connection between any single issuer's charter and their holdings is invisible at their scale. Voice runs only through intermediary stewards whose incentives are mixed.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, retail_pension_participants, payer,
    powerless, biographical, trapped, national).

% Produce voting analyses and policy frameworks consumed by institutional holders, recommending against super-voting structures at offering and for sunset and declassification provisions afterward. Revenue arrives from both investor-side subscriptions and issuer-paid research distribution. Recommendations bind only where votes are contested; inside controlled companies the result is predetermined regardless of advice.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, proxy_advisory_firms, observer,
    organized, biographical, mobile, global).

% Regulate disclosure quality, antifraud, and offering mechanics rather than voting architecture. Have examined concentrated-control structures repeatedly through concept releases, advisory-committee reports, and comment solicitations without adopting structural rules. Issuer choice of incorporation state and listing venue constrains the reachable levers to process, not vote allocation.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, sec_regulators, observer,
    institutional, generational, analytical, national).

% Buy shares in the aftermarket years after the offering that fixed the charter architecture, at prices embedding a control arrangement they were never party to. Entry offers acceptance or abstention only; renegotiation of the vote map is closed to them. Individually negligible, their collective position expresses itself only through index inclusion decisions they do not control.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, future_class_a_purchasers, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__minority_extraction, dual_class_founder_insiders).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__minority_extraction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the founder-capital time-horizon problem: lets a founding group commit multi-year strategy — platform reinvestment, long-payoff research bets, mission-bearing editorial lines — while raising public equity, by insulating decision rights from quarterly repricing, activist campaigns, and hostile bids. Secondarily provides a standardized, liquid vehicle through which dispersed savers can hold enterprises that would otherwise remain founder-locked.
% TRANSFER_FUNCTION: Moves governance value from public purchasers to the controlling bloc: each unit of Class A capital buys a fractionated voting claim while the founding group converts a small economic stake into decisive control. Controlled-company exemptions additionally move board-composition and oversight authority from independent-director mechanisms to the controlling holder. Private benefits of control — compensation ratification, related-party approval, succession designation, takeover-negotiation authority — concentrate correspondingly at the bloc.
% ABSENT_VOICES: Future purchasers had no seat at the consent moment that fixed the architecture; ultimate beneficial owners speak only through intermediaries whose fee relationships discourage adversarial stewardship; employees holding compensation equity sit outside charter negotiations entirely; venues that declined the architecture left the relevant market rather than remaining to argue. Listing-rule consultations are dominated by issuers, exchanges, and issuer counsel.
% DISAPPEARANCE_RATIONALE: Overnight enforcement of capital-proportional governance would force repricing or conversion across the controlled-company population, reopen board composition at exempt issuers, redistribute control premia from founder blocs to public holders, and redirect offering design toward single-class structures. Index construction, stewardship budgets, proxy-advisor policy frameworks, and Delaware fiduciary doctrine for controlled companies would all reorganize around the new baseline.
% FOUNDING_PROBLEM: Early twentieth-century founder and family proprietors wanted public capital without surrendering directional control: newspapers preserving editorial independence, industrialists guarding long-payoff investments against raiders and short-horizon markets. The dual-class vote map was built so outsiders could supply the money while insiders kept the wheel.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the academic corporate-governance and empirical-finance literature attests the time-horizon problem is real while returning mixed evidence on whether this architecture nets out positive for public holders; investor-protection bodies such as the Council of Institutional Investors and the International Corporate Governance Network attest that entrenchment harms remain live; exchange rulemaking records and the Hong Kong and Singapore listing-reform consultation papers attest that multiple jurisdictions felt forced to choose between the problem and the architecture. No party outside the founder bloc attests that the founding problem is simply solved.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__minority_extraction, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__minority_extraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__minority_extraction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dual_class_legitimacy__minority_extraction, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__minority_extraction, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is high (0.72) because the vote map decouples voting claims from capital at risk: public dollars buy fractionated governance while a small insider stake converts to decisive control, and the private-benefits literature documents measurable control rents (compensation ratification, related-party approval, succession designation, capture of takeover-negotiation authority). Suppression (0.67) reflects the enforcement machinery the arrangement requires — grandfathering of legacy structures, exemption administration, classified boards, and the competitive dynamics that punish any single venue for tightening — not participant preference. Theater (0.55) rises because a growing share of governance activity inside controlled companies is performative: annual meetings with predetermined outcomes, engagement sessions that cannot change vote-weight arithmetic, stewardship rituals addressed to a counterparty immune to them; audits, disclosure, and offering-process functions remain real. Accessibility collapse is 0.6: market-level alternatives survive (single-class investable firms exist, and index exclusions preserve some exit at the portfolio edge), but inside any controlled company alternatives collapse to selling at a governance discount or holding silently. Resistance is 0.6: two decades of institutional campaigns, proxy-advisor policies, index-provider exclusions, and cross-jurisdictional debates produced perimeter reforms (S&P exclusion of new dual-class entrants, sunset-clause norms in new offerings) without unwinding any legacy structure. Temporal series run on one shared eight-point grid (every tracked metric authored at every point; terminal point tagged projected); extraction accumulated fastest around T12-T15 (zero-vote experimentation, weighted-voting-rights adoption spreading across venues, exposed excesses at marquee failed offerings), then plateaued as sunset norms emerged while the legacy stock persisted. Suppression_requirement is traced here because the enforcement picture is dynamic — the machinery ratcheted up as adoption normalized — not static.
 *
 * PERSPECTIVAL GAP:
 *   The founder seat experiences the architecture as self-authored stewardship infrastructure: from inside, the arrangement computes as protective coordination it built and pays for, with effective extraction damped toward subsidy by beneficiary directionality. The trapped institutional and retail payer seats experience the same structure as full-price capital with fractionated voice and stripped protections — extraction-heavy. The exchange seat sits near-symmetric administratively: rule administration under competitive constraint, collecting listing fees either way. The regulator seat sees process integrity where the payer seats see architecture. The engine computes this per-seat divergence from the structural declarations; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Founder insiders, incumbent boards, and underwriters occupy the subsidized side of the arrangement (low d): the structure routes governance value and deal economics toward them. Trapped institutional managers and retail participants occupy the full-target end (high d): their exit is blocked by mandates and intermediary allocation respectively, and trapped targets sit nearer full-target than mobile ones. Exchanges and the securities regulator are administrative seats near symmetry — they run the machinery rather than collect its yield. Scope amplifies the extraction the payer seats experience: founder control and exemption effects verify poorly at the global scale on which these issuers operate, and dispersed holders bear the verification cost collectively. No directionality_overrides are authored: the override mechanism keys on power atoms, and the institutional atom is shared by opposed seats (exchanges versus asset managers), so a coarse override would misdirect; the beneficiary/victim declarations plus differentiated exit options carry the differentiation instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against a double error. Calling the arrangement pure coordination erases the documented transfer of governance value and the risk-without-voice condition of Class A holders; calling it pure extraction erases the genuine time-horizon problem the architecture answers and the real service it renders (public liquidity for founder-locked enterprises). Tangled rope preserves both facts: the coordination function is real, extraction is layered onto it through the same structure, and the enforcement load is load-bearing rather than vestigial. On genealogy: the founding problem (public capital without surrender of directional control) is contested-live — the short-horizon-market pressure it answered still exists, but the arrangement has generalized from editorial-independence cases into default control retention, and the mismatch consumer reads contested status against a world_rearranges verdict, so no zombie flag fires. Mandatrophy is not resolved; it is contested, which is the honest state for a structure whose defenders and targets disagree about which function is primary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This file instantiates one reading (minority_extraction) of the kernel dual_class_legitimacy; which reading''s structural declarations should govern classification of the standing dual-class arrangement?',
    'Corpus-level comparison across the three linked sibling stories (founder_stewardship, disclosure_consent): each fixes the same standing referent and authors a reading-indexed epsilon over it; divergence in computed per-seat types localizes the contest.',
    'Under founder_stewardship the same arrangement authors low epsilon and computes as protective coordination; under disclosure_consent epsilon collapses toward zero absent disclosure defect; this file authors high epsilon and extraction-forward structure. Authored reading_relations assert foreclosure of both siblings; engine-computed foreclosure may refine those edges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame routing: one of three mutually exclusive readings governs the classification surface.').

omega_variable(
    control_rent_magnitude,
    'How large are the private benefits of control actually captured by founder blocs (compensation capture, related-party transactions, entrenchment premia, foregone takeover premiums) relative to the coordination value delivered?',
    'Private-benefits econometrics: control-block premium studies, matched dual-class versus single-class valuations, abnormal-return studies around conversion and sunset-trigger events.',
    'Large verified rents push effective extraction toward snare territory for trapped seats; negligible rents would soften the arrangement toward genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_rent_magnitude, empirical, 'Empirical magnitude of control rents versus coordination value.').

omega_variable(
    stewardship_value_offset,
    'Does founder insulation produce net long-horizon value for public holders sufficient to offset the governance loss they bear?',
    'Matched-sample performance studies of dual-class versus single-class firms; natural experiments at sunset conversions comparing pre- and post-conversion outcomes.',
    'Net-positive offsets would justify a heavier coordination weighting; net-zero or negative outcomes confirm extraction dominance and raise the snare-flavored pressure on payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stewardship_value_offset, empirical, 'Whether the long-horizon benefit story nets out against the voice loss.').

omega_variable(
    trapped_passive_coalition_capacity,
    'Can index-manager coalitions convert their aggregate voting mass into effective voice despite fee-conflict paralysis and tracking mandates?',
    'Track voting outcomes on sunset, declassification, and conversion proposals at controlled companies; monitor stewardship-policy shifts at the largest managers and their willingness to vote against controlling slates.',
    'Demonstrated coalition wins would lower suppression and raise resistance, moving the arrangement''s trajectory toward reform; persistent futility entrenches the trap and supports snare drift at payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trapped_passive_coalition_capacity, empirical, 'Coalition capacity of trapped passive holders.').

omega_variable(
    sunset_clause_sincerity,
    'Are the sunset clauses newly standard in dual-class offerings a genuine transitional commitment or a theatrical concession that preserves indefinite control?',
    'Compare adopted clause parameters (window length, trigger events, cap resets) against actual conversion compliance across offering cohorts; audit trigger-event handling when windows expire.',
    'Sincere sunsets introduce scaffold-like transition dynamics into parts of the population; sham clauses inflate theater_ratio further and confirm entrenchment as the operative function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_sincerity, empirical, 'Sincerity of sunset-clause concessions in recent offerings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__minority_extraction, 0, 21).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__minority_extraction, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(dual_tr_t0, observed).
narrative_ontology:measurement(dual_tr_t3, dual_class_legitimacy__minority_extraction, theater_ratio, 3, 0.38).
narrative_ontology:measurement_basis(dual_tr_t3, observed).
narrative_ontology:measurement(dual_tr_t6, dual_class_legitimacy__minority_extraction, theater_ratio, 6, 0.41).
narrative_ontology:measurement_basis(dual_tr_t6, observed).
narrative_ontology:measurement(dual_tr_t9, dual_class_legitimacy__minority_extraction, theater_ratio, 9, 0.44).
narrative_ontology:measurement_basis(dual_tr_t9, observed).
narrative_ontology:measurement(dual_tr_t12, dual_class_legitimacy__minority_extraction, theater_ratio, 12, 0.48).
narrative_ontology:measurement_basis(dual_tr_t12, observed).
narrative_ontology:measurement(dual_tr_t15, dual_class_legitimacy__minority_extraction, theater_ratio, 15, 0.52).
narrative_ontology:measurement_basis(dual_tr_t15, observed).
narrative_ontology:measurement(dual_tr_t18, dual_class_legitimacy__minority_extraction, theater_ratio, 18, 0.54).
narrative_ontology:measurement_basis(dual_tr_t18, observed).
narrative_ontology:measurement(dual_tr_t21, dual_class_legitimacy__minority_extraction, theater_ratio, 21, 0.55).
narrative_ontology:measurement_basis(dual_tr_t21, projected).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__minority_extraction, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(dual_be_t0, observed).
narrative_ontology:measurement(dual_be_t3, dual_class_legitimacy__minority_extraction, base_extractiveness, 3, 0.58).
narrative_ontology:measurement_basis(dual_be_t3, observed).
narrative_ontology:measurement(dual_be_t6, dual_class_legitimacy__minority_extraction, base_extractiveness, 6, 0.61).
narrative_ontology:measurement_basis(dual_be_t6, observed).
narrative_ontology:measurement(dual_be_t9, dual_class_legitimacy__minority_extraction, base_extractiveness, 9, 0.64).
narrative_ontology:measurement_basis(dual_be_t9, observed).
narrative_ontology:measurement(dual_be_t12, dual_class_legitimacy__minority_extraction, base_extractiveness, 12, 0.69).
narrative_ontology:measurement_basis(dual_be_t12, observed).
narrative_ontology:measurement(dual_be_t15, dual_class_legitimacy__minority_extraction, base_extractiveness, 15, 0.71).
narrative_ontology:measurement_basis(dual_be_t15, observed).
narrative_ontology:measurement(dual_be_t18, dual_class_legitimacy__minority_extraction, base_extractiveness, 18, 0.71).
narrative_ontology:measurement_basis(dual_be_t18, observed).
narrative_ontology:measurement(dual_be_t21, dual_class_legitimacy__minority_extraction, base_extractiveness, 21, 0.72).
narrative_ontology:measurement_basis(dual_be_t21, projected).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__minority_extraction, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(dual_su_t0, observed).
narrative_ontology:measurement(dual_su_t3, dual_class_legitimacy__minority_extraction, suppression_requirement, 3, 0.53).
narrative_ontology:measurement_basis(dual_su_t3, observed).
narrative_ontology:measurement(dual_su_t6, dual_class_legitimacy__minority_extraction, suppression_requirement, 6, 0.56).
narrative_ontology:measurement_basis(dual_su_t6, observed).
narrative_ontology:measurement(dual_su_t9, dual_class_legitimacy__minority_extraction, suppression_requirement, 9, 0.6).
narrative_ontology:measurement_basis(dual_su_t9, observed).
narrative_ontology:measurement(dual_su_t12, dual_class_legitimacy__minority_extraction, suppression_requirement, 12, 0.65).
narrative_ontology:measurement_basis(dual_su_t12, observed).
narrative_ontology:measurement(dual_su_t15, dual_class_legitimacy__minority_extraction, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(dual_su_t15, observed).
narrative_ontology:measurement(dual_su_t18, dual_class_legitimacy__minority_extraction, suppression_requirement, 18, 0.66).
narrative_ontology:measurement_basis(dual_su_t18, observed).
narrative_ontology:measurement(dual_su_t21, dual_class_legitimacy__minority_extraction, suppression_requirement, 21, 0.67).
narrative_ontology:measurement_basis(dual_su_t21, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__minority_extraction, resource_allocation).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% The colloquial label 'dual-class legitimacy' conflates three structurally distinct claims about the same standing arrangement (super-voting founder classes plus controlled-company exemptions), so per the epsilon-invariance principle it decomposes into a three-story constraint family: minority_extraction (this file), founder_stewardship, and disclosure_consent. Each story fixes the identical referent and authors a reading-indexed epsilon over it; the epsilons differ by construction because each reading evaluates a different constraint. Lineage within the family: the disclosure-consent reading is upstream — the Securities Act consent apparatus supplies the legitimacy vocabulary the other two readings contest; this reading's entitlement claim generates the enforcement and reform pressure that the stewardship reading answers defensively. All three files cross-link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
