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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Dual-Class Voting Structure — Minority Extraction Reading (Governance Proportional to Capital and Risk)
 *   domain: economic/legal/corporate_governance
 *
 * SUMMARY:
 *   A public company issues two or more classes of common stock: founders and
 *   early insiders hold supervoting shares carrying multiple votes per share,
 *   while public investors hold one-vote shares at the same economic risk per
 *   dollar. The standing arrangement under contest — the referent for every
 *   metric in this story — is that dual-class governance structure as it
 *   actually operates at listed firms: the controller fixes board composition
 *   and charter terms with a minority of the capital, public capital bears
 *   pro-rata downside with minority voice, controlled-company exemptions
 *   waive exchange requirements for majority-independent boards and
 *   independent compensation committees, and no proposal or director slate
 *   can pass without controller consent. This story instantiates the
 *   minority_extraction reading of the kernel dual_class_legitimacy:
 *   governance value is transferred from public capital to the controlling
 *   block, and minority shareholders bear risk without commensurate voice.
 *   The sibling readings (founder_stewardship, disclosure_consent) assess the
 *   same charters as beneficial coordination or as consented exchange and are
 *   authored as separate constraint stories linked through the network
 *   section. KEY AGENTS (by structural relationship):
 *   founder_controlling_holders — agenda-setter and primary beneficiary
 *   (institutional/arbitrage); public_class_a_shareholders — primary target
 *   (powerless/mobile); passive_index_savers — deepest target
 *   (powerless/trapped); early_vc_insider_holders — secondary beneficiary
 *   (powerful/arbitrage); insider_directors — beneficiary (powerful/mobile);
 *   listing_exchanges — co-administrator and beneficiary
 *   (institutional/arbitrage); index_fund_managers — dual-positioned
 *   intermediary (institutional/constrained); activist_investors — excluded
 *   (powerful/mobile); securities_regulators and proxy_advisory_firms —
 *   observers (institutional/analytical, organized/analytical).
 *
 * KEY AGENTS:
 *   - founder_controlling_holders: agenda-setter and primary beneficiary (institutional/arbitrage) — supervoting block, minority capital, charter and board control
 *   - public_class_a_shareholders: primary target (powerless/mobile) — pro-rata economic risk, one vote per share, liquid exit at a governance-discounted price
 *   - passive_index_savers: deepest target (powerless/trapped) — hold through index and retirement defaults, cannot exit without abandoning market exposure
 *   - early_vc_insider_holders: secondary beneficiary (powerful/arbitrage) — governance rights beyond current capital, monetizable at liquidity events
 *   - insider_directors: beneficiary (powerful/mobile) — board seats and compensation insulated from election contests
 *   - listing_exchanges: co-administrator and beneficiary (institutional/arbitrage) — listing standards admit the structure; fee and competition incentives hold standards down
 *   - index_fund_managers: dual-positioned intermediary (institutional/constrained) — must hold the shares for fee income while stewardship policy presses against the structure
 *   - activist_investors: excluded (powerful/mobile) — voice foreclosed by vote math; remedy limited to exit and publicity
 *   - securities_regulators: observer (institutional/analytical) — disclosure-only posture after the 1990 precedent
 *   - proxy_advisory_firms: observer (organized/analytical) — against-recommendations without authority to pass anything
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__minority_extraction, 0.74).
domain_priors:suppression_score(dual_class_legitimacy__minority_extraction, 0.62).
domain_priors:theater_ratio(dual_class_legitimacy__minority_extraction, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, extractiveness, 0.74).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__minority_extraction, tangled_rope).
narrative_ontology:human_readable(dual_class_legitimacy__minority_extraction, "Dual-Class Voting Structure — Minority Extraction Reading (Governance Proportional to Capital and Risk)").
narrative_ontology:topic_domain(dual_class_legitimacy__minority_extraction, "economic/legal/corporate_governance").

domain_priors:requires_active_enforcement(dual_class_legitimacy__minority_extraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__minority_extraction, 'f47d0e9a-c70c-452f-8b41-ddbf8f547fda').
narrative_ontology:cs_kernel_codification('f47d0e9a-c70c-452f-8b41-ddbf8f547fda', formalized).
narrative_ontology:cs_authority_grounding('f47d0e9a-c70c-452f-8b41-ddbf8f547fda', lineage).
narrative_ontology:cs_interpretation_layer_present('f47d0e9a-c70c-452f-8b41-ddbf8f547fda').
narrative_ontology:cs_reading_relation('f47d0e9a-c70c-452f-8b41-ddbf8f547fda', dual_class_legitimacy__founder_stewardship, influences).
narrative_ontology:cs_reading_relation('f47d0e9a-c70c-452f-8b41-ddbf8f547fda', dual_class_legitimacy__disclosure_consent, coexists_with).
narrative_ontology:cs_axiom('f47d0e9a-c70c-452f-8b41-ddbf8f547fda', foundational, governance_proportional_to_capital_risk).
narrative_ontology:cs_axiom_status(governance_proportional_to_capital_risk, holdable).
narrative_ontology:cs_axiom_grounding('f47d0e9a-c70c-452f-8b41-ddbf8f547fda', governance_proportional_to_capital_risk, deontological).
narrative_ontology:cs_axiom('f47d0e9a-c70c-452f-8b41-ddbf8f547fda', secondary, control_risk_decoupling_is_agency_cost).
narrative_ontology:cs_axiom_status(control_risk_decoupling_is_agency_cost, holdable).
narrative_ontology:cs_axiom_grounding('f47d0e9a-c70c-452f-8b41-ddbf8f547fda', control_risk_decoupling_is_agency_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('f47d0e9a-c70c-452f-8b41-ddbf8f547fda', one_share_one_vote_baseline).
narrative_ontology:cs_drift_state('f47d0e9a-c70c-452f-8b41-ddbf8f547fda', contemporary_dual_class_wave, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f47d0e9a-c70c-452f-8b41-ddbf8f547fda', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__minority_extraction, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, founder_controlling_holders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, early_vc_insider_holders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, insider_directors).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, listing_exchanges).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, public_class_a_shareholders).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, passive_index_savers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, index_fund_managers).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, index_fund_managers).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__minority_extraction, charter_contractarianism).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__minority_extraction, controlled_company_exemption_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold super-voting Class B shares (commonly 5-10 votes per share) obtained at founding, controlling 55-70% of board votes with roughly 5-15% of economic capital. Wrote the charter that fixes the vote ratio; nominate and remove directors; set executive compensation through boards they control; can sell converted Class A shares into the market while keeping control. Economic downside is pro rata to their stake; decision authority is not. Any charter change requires their own votes.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, founder_controlling_holders, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__minority_extraction, founder_controlling_holders, beneficiary).

% Took supervoting or founder-aligned shares in early rounds; after later dilution and partial selling they hold governance rights well beyond their current economic stake. They can distribute or sell converted shares at liquidity events while the supervoting block they sit inside persists. Returns track company performance; influence persists past their capital.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, early_vc_insider_holders, beneficiary,
    powerful, biographical, arbitrage, global).

% Occupy board seats whose composition the controlling block fixes. Collect director compensation and committee assignments with no realistic election contest, since no rival slate can reach a winning vote count. Tenure runs on the controller's support rather than on shareholder elections.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, insider_directors, beneficiary,
    powerful, biographical, mobile, global).

% Write and administer the listing standards that admit dual-class structures, and compete with each other to host the high-profile listings that carry them; listing fees and trading revenue follow. After dropping their own one-share-one-vote requirements in the 1980s, neither major exchange can tighten standards unilaterally without losing listings to the other, so both settled for disclosure-based rules such as listed-company agreements requiring conversion of supervoting shares on transfer.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, listing_exchanges, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__minority_extraction, listing_exchanges, agenda_setter).

% Buy at the public price and bear economic downside pro rata to capital contributed, with one vote per share against the controller's many. They can attend annual meetings and submit proposals, but nothing passes without the controller's consent. They can sell in a liquid market at a price that embeds a modest discount for the surrendered voting rights.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, public_class_a_shareholders, payer,
    powerless, biographical, mobile, global).

% Hold these companies through index funds and retirement-plan defaults they did not pick share by share. Their outcomes depend on firms whose governance they cannot influence and which they cannot exit without abandoning broad market exposure. Their voice is delegated to the asset managers who vote the shares.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, passive_index_savers, payer,
    powerless, biographical, trapped, national).

% Must hold dual-class constituents inside index-tracking products whose fee income depends on inclusion, while administering stewardship policies that increasingly call for sunset clauses and against-votes on dual-class structures. They face issuer-relationship and distribution-channel pressure on one side and beneficiary and regulatory pressure on the other. Refusing to hold means abandoning index products; voting for reform cannot win while the supervoting block exists.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, index_fund_managers, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__minority_extraction, index_fund_managers, payer).

% Would run board contests, propose sunset clauses, and campaign for vote parity, but cannot assemble a winning vote count against the supervoting block. Their usual lever — accumulate a stake and force change — is unavailable here; their effective options are public criticism and exit. Some still take positions for valuation catalysts short of governance change.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, activist_investors, excluded,
    powerful, biographical, mobile, global).

% Require disclosure of the structure in prospectuses and proxy statements, study the arrangement, and publish requests for comment; they declined to impose structural rules after the 1990 appellate decision struck down their one-share-one-vote rule. Their authority over listing standards runs through the exchanges' self-regulatory status and that precedent.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, securities_regulators, observer,
    institutional, generational, analytical, national).

% Issue voting recommendations that institutional investors largely follow; recommend against dual-class structures and for sunset clauses. They pass nothing themselves; their influence runs through managers who cannot win the contests they recommend for.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, proxy_advisory_firms, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__minority_extraction, founder_controlling_holders).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__minority_extraction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates decision authority in the founder/controlling block, insulating management from hostile takeover and short-term market pressure so that long-horizon, firm-specific investments and mission commitments can be executed without capital-market discipline overriding them; solves the incumbent-management side of the market-for-corporate-control problem.
% TRANSFER_FUNCTION: Moves governance value — voting power, board composition control, agenda-setting power, takeover protection — from public Class A capital to the super-voting Class B block, while economic risk remains allocated pro rata to capital contributed; additionally moves private-control benefits (compensation setting, related-party transaction approval, entrenchment) to the controlling insiders.
% ABSENT_VOICES: Activist investors and governance reformers are structurally excluded: proxy contests cannot succeed against super-voting blocks, so their objections never reach a deciding forum, and their only remedies are exit and publicity. Passive index savers are present only through asset-manager proxies whose fee and business incentives diverge from saver interests. Minority co-founders diluted at recapitalization and minority holders of controlled subsidiaries are outside the conversation entirely.
% DISAPPEARANCE_RATIONALE: If dual-class structures and the entitlement they embody vanished overnight, capital allocation would rearrange: founders would fund growth through private capital or accept control parity, control premiums would reprice into single-class charters, the takeover market for mega-cap firms would reopen, index construction and stewardship voting would simplify, and the standing control arrangements at several of the largest listed firms would unwind at the next conversion trigger or sale.
% FOUNDING_PROBLEM: The vulnerability of founder-led, long-horizon enterprises to hostile takeover and quarterly market pressure once they access public capital — the problem dual-class structures, and before them media-family voting trusts, were built to solve.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: institutional-investor coalitions (e.g., the Council of Institutional Investors) and proxy advisors attest the original takeover-discipline problem was real but is now invoked selectively; the academic corporate-law literature (Bebchuk and the dual-class empirical line) documents that vulnerability declines after listing while structures persist indefinitely — Alphabet remains dual-class two decades post-IPO; index providers' eligibility policies (S&P's 2017 bar on new dual-class entrants) constitute institutional attestation that the structure is not justified by the founding problem. No source outside the beneficiary set attests that the founding problem justifies indefinite, sunset-free duration.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__minority_extraction, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__minority_extraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__minority_extraction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dual_class_legitimacy__minority_extraction, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__minority_extraction, 0.74, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored high (0.74) because the arrangement's defining flow is a governance-value transfer decoupled from capital and risk: the controller converts a 5-15% economic stake into 55-70% of decision authority, sets compensation through boards it appoints, and holds charter-lock that forecloses internal correction, while the public side bears pro-rata downside with one vote per share. Suppression (0.62) is authored as a raw structural property — it is NOT scaled by power or scope; only extractiveness is scaled, by directionality and scope in the engine — and it is structural rather than internalized: proxy futility is arithmetic (a 10:1 vote ratio cannot be out-organized), charter supermajorities and controller consent gate every internal remedy, and controlled-company exemptions strip the listing-level protections that would otherwise apply. Exit is not suppressed — shares are liquid — which is why the arrangement suppresses voice rather than exit, and why resistance routes around the firm to index providers, regulators, and norm entrepreneurs. Theater (0.48) reflects the growing share of governance activity that is performative: annual meetings whose outcomes are fixed before they convene, engagement programs that produce no binding change, independent-director facades at controlled companies exempt from the independence requirements. Accessibility_collapse (0.62): once the structure is understood, the alternative — proportional governance in these firms — is unavailable by construction, and index inclusion forecloses avoidance while retaining market exposure; the residual accessibility is the liquid exit itself. Resistance (0.62) is real and sustained — the S&P 2017 eligibility bar on new dual-class entrants, proxy-advisor against-recommendations, institutional-investor coalitions, the academic critique — and has constrained new adoption while unwinding almost no standing structure; coalition potential among diffuse holders is neutralized by the vote math, which is precisely why the resistance that matters is institutional rather than in-meeting. The three measurement series run on one shared time grid (points 0, 8, 16, 24, 32, 40 of an interval mapping 1984-2024) so every tracked metric is authored at every examined time point; series end values equal the base_properties scalars. The suppression_requirement series is authored because the story specifically tracks enforcement-capacity change: the exchange-level protection stripping (controlled-company exemptions), Delaware deference hardening, and charter-lock machinery matured over the interval. claimed_type is authored as tangled_rope from this reading's seat — the structure retains a genuine coordination function (insulating long-horizon decisions from takeover pressure) while enforcing an asymmetric transfer through the same charter machinery — and the metrics are authored as independent descriptive facts; the engine computes per-seat types from the structural data, and divergence between claim and computed type is the measurement, not an error to reconcile.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently. From the controller's seat the arrangement is coordination it built and maintains: insulation from quarterly pressure, authority matched to firm-specific knowledge, mission continuity — the transfer experienced as earned. From the public Class A seat the same charter arithmetic is risk without voice: full downside participation, minority say, and an exit priced at a discount that does not return the surrendered governance value. From the passive saver's seat the experience is deeper still — trapped channel exposure with voice delegated to managers whose fee incentives cut against exercising it. The index manager's seat is genuinely ambivalent: it collects fee income from inclusion while its stewardship policy presses against the structure, and it can neither exit the holding nor win the vote. The excluded activist seat experiences foreclosed voice — the arrangement does not need to suppress its exit, only its vote count. The engine computes these per-seat classifications from the structural data; this story does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (founder_controlling_holders, early_vc_insider_holders, insider_directors, listing_exchanges) drive d toward the beneficiary end for those seats: the controller collects the governance premium directly (the gain_flow seat), the exchanges collect listing revenue from admitting the structure, insiders collect insulated board compensation. Victim declarations (public_class_a_shareholders, passive_index_savers) drive d toward the target end. One directionality override is authored: both powerless seats are declared victims, but the derivation would damp the direct holder's d for mobile exit; the damp is wrong here because the exit price embeds only a partial governance-value discount and the index channel forecloses exit for the saver seat entirely — the true target position for both powerless seats sits near full-target (0.85). The override is keyed to the power atom both victim seats share, and no beneficiary seat carries that atom, so the override cannot misfire on a beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification guards against two mislabels. A pure-extraction mislabel would erase the genuine coordination function the structure performs — insulating long-horizon decisions from takeover pressure is a real collective-action service, and any remedy that ignores it will misprice the transition. A pure-coordination mislabel would erase the asymmetric transfer the same charter machinery enforces. The mandatrophy question is acute because the founding problem is time-bound: takeover vulnerability is sharpest at and shortly after listing and decays as the firm matures, yet most charters carry no sunset — the justification is transitional while the arrangement is standing. The R5 interview records that mismatch as contested rather than resolved: whether stewardship value continues to offset the transfer after the vulnerable window is an empirical question (omega stewardship_value_offset), and whether sunset norms will convert the marginal structure into a genuinely transitional form is open (omega sunset_norm_diffusion). If the founding problem is dead and the arrangement persists, classification drifts toward the degraded/inertial pole with theatrical maintenance (the theater_ratio series tracks exactly that); if it is live, the coordination component holds and the hybrid classification stands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_divergence,
    'This story is one reading of the kernel dual_class_legitimacy. Would the sibling readings (founder_stewardship, disclosure_consent) compute a different epsilon and type for the same charters, and does the corpus need all three to classify the domain?',
    'Author the sibling stories over the same structural data and compare per-reading epsilon and computed type; the disagreement is located in the legitimacy criterion (proportionality vs. stewardship service vs. informed consent), not in any empirical fact about the charters.',
    'Under disclosure_consent, epsilon drops and the arrangement computes coordination-dominant; under founder_stewardship, the transfer is re-read as service. Only the minority_extraction seat yields the high-extraction profile authored here.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Committer-frame omega: reading-indexed classification over a shared kernel.').

omega_variable(
    control_premium_compensation,
    'How much of the governance value surrendered by public buyers is returned in the price they pay — do dual-class shares price at a discount that fully compensates for the lost votes?',
    'Event-study and matched-firm pricing of dual-class versus single-class IPOs and secondaries; application of the vote-premium literature to the actual class ratios and lock-up terms.',
    'Full price compensation would re-author epsilon materially lower (a disclosed, priced exchange); partial compensation — the empirical pattern to date — leaves the transfer standing as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_premium_compensation, empirical, 'Whether the IPO/secondary-market discount compensates Class A holders for surrendered governance value.').

omega_variable(
    stewardship_value_offset,
    'Does founder control generate long-horizon value that partially offsets the governance transfer for public holders — the founder_stewardship reading''s empirical core?',
    'Matched-firm long-horizon performance and investment-horizon studies of dual-class versus single-class firms, controlling for selection at IPO.',
    'A material offset would shift the hybrid balance toward coordination and lower effective extraction; a negligible offset would push the arrangement toward the pure-extraction pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stewardship_value_offset, empirical, 'Whether stewardship benefits offset the governance-value transfer.').

omega_variable(
    sunset_norm_diffusion,
    'Will time-based sunset clauses (post-2020 IPO charters) diffuse to the point where new dual-class structures are genuinely transitional rather than standing?',
    'Track charter provisions of IPO cohorts 2020-2035: sunset prevalence, trigger design (time versus dilution versus founder departure), and enforcement.',
    'Rising sunsets would reclassify the marginal structure toward the transitional type and date the standing-arrangement profile authored here to the legacy cohort; stagnant sunsets confirm the standing arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_norm_diffusion, empirical, 'Whether sunset diffusion converts the arrangement from standing to transitional.').

omega_variable(
    index_lockin_attribution,
    'Is the passive saver''s inability to exit a feature of this governance arrangement or of the retirement-system architecture (plan defaults, index-fund structure) that channels savings into it — separable constraints or one?',
    'Compare exit costs under alternative channel architectures: governance-screened index products, equal-cost exclusion funds, or voting pass-through mechanisms; if saver exit costs are unchanged by governance reform, the trap belongs to the channel, not the charter.',
    'If separable, the suppression authored against the saver seat is overstated here and belongs to a separate channel story; if inseparable, the charter exploits the channel and the authored profile stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(index_lockin_attribution, conceptual, 'Whether the passive saver''s trapped exit is attributable to this arrangement or to the retirement-channel architecture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__minority_extraction, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__minority_extraction, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(dual_tr_t0, observed).
narrative_ontology:measurement(dual_tr_t8, dual_class_legitimacy__minority_extraction, theater_ratio, 8, 0.3).
narrative_ontology:measurement_basis(dual_tr_t8, observed).
narrative_ontology:measurement(dual_tr_t16, dual_class_legitimacy__minority_extraction, theater_ratio, 16, 0.35).
narrative_ontology:measurement_basis(dual_tr_t16, observed).
narrative_ontology:measurement(dual_tr_t24, dual_class_legitimacy__minority_extraction, theater_ratio, 24, 0.4).
narrative_ontology:measurement_basis(dual_tr_t24, observed).
narrative_ontology:measurement(dual_tr_t32, dual_class_legitimacy__minority_extraction, theater_ratio, 32, 0.45).
narrative_ontology:measurement_basis(dual_tr_t32, observed).
narrative_ontology:measurement(dual_tr_t40, dual_class_legitimacy__minority_extraction, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(dual_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__minority_extraction, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(dual_be_t0, observed).
narrative_ontology:measurement(dual_be_t8, dual_class_legitimacy__minority_extraction, base_extractiveness, 8, 0.6).
narrative_ontology:measurement_basis(dual_be_t8, observed).
narrative_ontology:measurement(dual_be_t16, dual_class_legitimacy__minority_extraction, base_extractiveness, 16, 0.63).
narrative_ontology:measurement_basis(dual_be_t16, observed).
narrative_ontology:measurement(dual_be_t24, dual_class_legitimacy__minority_extraction, base_extractiveness, 24, 0.68).
narrative_ontology:measurement_basis(dual_be_t24, observed).
narrative_ontology:measurement(dual_be_t32, dual_class_legitimacy__minority_extraction, base_extractiveness, 32, 0.72).
narrative_ontology:measurement_basis(dual_be_t32, observed).
narrative_ontology:measurement(dual_be_t40, dual_class_legitimacy__minority_extraction, base_extractiveness, 40, 0.74).
narrative_ontology:measurement_basis(dual_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__minority_extraction, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(dual_su_t0, observed).
narrative_ontology:measurement(dual_su_t8, dual_class_legitimacy__minority_extraction, suppression_requirement, 8, 0.45).
narrative_ontology:measurement_basis(dual_su_t8, observed).
narrative_ontology:measurement(dual_su_t16, dual_class_legitimacy__minority_extraction, suppression_requirement, 16, 0.5).
narrative_ontology:measurement_basis(dual_su_t16, observed).
narrative_ontology:measurement(dual_su_t24, dual_class_legitimacy__minority_extraction, suppression_requirement, 24, 0.56).
narrative_ontology:measurement_basis(dual_su_t24, observed).
narrative_ontology:measurement(dual_su_t32, dual_class_legitimacy__minority_extraction, suppression_requirement, 32, 0.6).
narrative_ontology:measurement_basis(dual_su_t32, observed).
narrative_ontology:measurement(dual_su_t40, dual_class_legitimacy__minority_extraction, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(dual_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__minority_extraction, enforcement_mechanism).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% The colloquial label 'dual-class legitimacy' decomposes into three structurally distinct claims per the epsilon-invariance principle: this story (minority_extraction), dual_class_legitimacy__founder_stewardship, and dual_class_legitimacy__disclosure_consent. All three take the standing dual-class arrangement as referent and differ in reading-indexed epsilon; they form a constraint family linked here. This reading exerts structural pressure on the stewardship reading (index eligibility bars and sunset norms shrink its clean-case domain) while coexisting with the consent reading as rival live positions. The dual formulation note should appear in all three family files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dual_class_legitimacy__minority_extraction, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
