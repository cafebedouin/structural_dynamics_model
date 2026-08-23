% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__disclosure_consent
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_class_legitimacy__disclosure_consent, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: dual_class_legitimacy__disclosure_consent
 *   human_readable: Dual-Class Structure Legitimated by Informed Consent at Registration Disclosure
 *   domain: economic/legal/corporate_governance
 *
 * SUMMARY:
 *   Modern United States markets permit founders to take companies public
 *   retaining supervoting stock — typically ten votes per insider share
 *   against one per publicly sold share — provided the structure is fully
 *   disclosed in the S-1 registration statement. This story instantiates the
 *   disclosure_consent reading of the dual_class_legitimacy kernel: on this
 *   reading the standing arrangement is contractual choice rather than
 *   imposed order — the Securities Act disclosure process discharges the
 *   consent duty, investors purchase the low-vote shares with eyes open, and
 *   the governance disparity clears the market through price. The epsilon
 *   referent is the standing dual-class arrangement itself, assessed by this
 *   reading's own lights; it is not the consent-idealized counterfactual this
 *   reading would defend, and it is not averaged against sibling readings,
 *   which are separate constraint files linked in
 *   network.affects_constraints. The kernel-level contest is routed to the
 *   omega variables. KEY AGENTS (by structural relationship): -
 *   dual_class_founders: agenda-setting collector of the control differential
 *   (powerful/arbitrage) — designs, administers, and ratifies the structure -
 *   institutional_class_a_holders: compensated cost-bearing counterparties
 *   (organized/constrained) — benchmark-bound, governance-diluted -
 *   retail_class_a_investors: weakest consent seat (powerless/mobile) —
 *   maximal exit speed, zero voice - sec_registration_authority: disclosure
 *   administrator (institutional/constrained) - stock_exchange_operators:
 *   venue-competing fee collectors who set admitting standards
 *   (institutional/arbitrage) - underwriting_syndicates: per-deal fee
 *   collectors (institutional/arbitrage) - proxy_advisory_governance_bodies:
 *   objectors processed by a channel the structure controls
 *   (organized/trapped) - delaware_charter_courts: analytical observer
 *   adjudicating within the architecture
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__disclosure_consent, 0.28).
domain_priors:suppression_score(dual_class_legitimacy__disclosure_consent, 0.16).
domain_priors:theater_ratio(dual_class_legitimacy__disclosure_consent, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, extractiveness, 0.28).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, suppression_requirement, 0.16).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__disclosure_consent, rope).
narrative_ontology:human_readable(dual_class_legitimacy__disclosure_consent, "Dual-Class Structure Legitimated by Informed Consent at Registration Disclosure").
narrative_ontology:topic_domain(dual_class_legitimacy__disclosure_consent, "economic/legal/corporate_governance").

domain_priors:requires_active_enforcement(dual_class_legitimacy__disclosure_consent).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__disclosure_consent, '51b67b7d-f374-4d03-a4b1-1b7007b74e12').
narrative_ontology:cs_kernel_codification('51b67b7d-f374-4d03-a4b1-1b7007b74e12', formalized).
narrative_ontology:cs_authority_grounding('51b67b7d-f374-4d03-a4b1-1b7007b74e12', lineage).
narrative_ontology:cs_interpretation_layer_present('51b67b7d-f374-4d03-a4b1-1b7007b74e12').
narrative_ontology:cs_reading_relation('51b67b7d-f374-4d03-a4b1-1b7007b74e12', dual_class_legitimacy__founder_stewardship, influences).
narrative_ontology:cs_reading_relation('51b67b7d-f374-4d03-a4b1-1b7007b74e12', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_axiom('51b67b7d-f374-4d03-a4b1-1b7007b74e12', foundational, informed_disclosure_consents_suffice_for_legitimacy).
narrative_ontology:cs_axiom_status(informed_disclosure_consents_suffice_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('51b67b7d-f374-4d03-a4b1-1b7007b74e12', informed_disclosure_consents_suffice_for_legitimacy, conventional).
narrative_ontology:cs_axiom('51b67b7d-f374-4d03-a4b1-1b7007b74e12', secondary, governance_disparity_is_priced_into_valuation).
narrative_ontology:cs_axiom_status(governance_disparity_is_priced_into_valuation, holdable).
narrative_ontology:cs_axiom_grounding('51b67b7d-f374-4d03-a4b1-1b7007b74e12', governance_disparity_is_priced_into_valuation, empirically_contingent).
narrative_ontology:cs_reference_frame('51b67b7d-f374-4d03-a4b1-1b7007b74e12', registration_time_informed_consent_baseline).
narrative_ontology:cs_drift_state('51b67b7d-f374-4d03-a4b1-1b7007b74e12', contemporary_post_snap_retail_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('51b67b7d-f374-4d03-a4b1-1b7007b74e12', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, dual_class_founders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, institutional_class_a_holders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, retail_class_a_investors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, stock_exchange_operators).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, underwriting_syndicates).
narrative_ontology:constraint_victim(dual_class_legitimacy__disclosure_consent, institutional_class_a_holders).
narrative_ontology:constraint_victim(dual_class_legitimacy__disclosure_consent, retail_class_a_investors).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__disclosure_consent, informed_consent_via_statutory_disclosure).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__disclosure_consent, contractual_freedom_in_charter_design).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design the supervoting structure at incorporation, choose the listing venue, and retain roughly ten votes per insider share against one per publicly sold share while selling most of the economics to the public. Every ratification vote on the structure is decided by their own voting bloc. Exit is wide: convert, sell into strength, take private, or reincorporate in a friendlier jurisdiction.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, dual_class_founders, agenda_setter,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__disclosure_consent, dual_class_founders, beneficiary).

% Mutual funds and pension assets hold the low-vote shares for benchmark and growth exposure. Their votes are diluted ten-to-one, so sponsorship of sunset resolutions and board accountability proposals reliably fails. Selling out means tracking error and transaction costs; staying means holding a position whose governance weight they cannot exercise. They fund proxy-advisory research as their substitute voice.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, institutional_class_a_holders, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__disclosure_consent, institutional_class_a_holders, beneficiary).

% Buy through brokerage apps on the disclosed terms, gaining access to founder-led growth companies otherwise unavailable. Their votes are effectively nil and the registration-statement risk factors that describe the voting differential are rarely read in full. Exit is instant — they can sell any day — but voice is permanently zero while they hold.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, retail_class_a_investors, payer,
    powerless, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__disclosure_consent, retail_class_a_investors, beneficiary).

% Administers the registration-review process that defines what counts as adequate disclosure of the dual-class terms, reviews every S-1 for completeness, and after its 2019 dual-class roundtable chose to leave the structure to market choice rather than mandate changes. Its discretion is bounded by statute and commission politics.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, sec_registration_authority, agenda_setter,
    institutional, generational, constrained, national).

% Set listing standards that admit dual-class issuers, collect listing fees and index-linked volume from them, and in their joint 2018 comment letter declined to impose voting-rights restrictions, citing competition between venues for listings. Either venue could tighten standards unilaterally; each fears the issuer migrating to the rival.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, stock_exchange_operators, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__disclosure_consent, stock_exchange_operators, agenda_setter).

% Structure and place dual-class offerings, advise issuers on vote architecture and sunset clauses, and collect gross spreads on each deal. Their economics depend on continued issuer demand for the structure; they bear none of its downstream governance costs.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, underwriting_syndicates, beneficiary,
    institutional, immediate, arbitrage, global).

% Proxy advisers and investor coalitions publish policies recommending against dual-class structures and demanding sunset provisions. Their recommendations reach shareholder meetings where the insider voting bloc outnumbers them by construction; their objection is processed by a channel the very structure they oppose controls. They cannot leave the system — their clients hold the shares — only dissent inside it.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, proxy_advisory_governance_bodies, excluded,
    organized, biographical, trapped, national).

% Adjudicate control contests and fiduciary claims arising under dual-class charters, treating the structure as a valid contract struck among fully informed parties at the offering. They assess conduct within the chosen architecture, not the architecture's legitimacy itself.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, delaware_charter_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__disclosure_consent, dual_class_founders).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__disclosure_consent, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches founders who want public capital without surrendering long-horizon control to investors willing to accept subordinated voting power in exchange for growth-company access, with the S-1 process ensuring both sides strike the bargain from the same written terms.
% TRANSFER_FUNCTION: At the offering, transfers supermajority voting power from the public float to insider holders — Class A buyers surrender proportionate governance while founders retain it at economic cost below open-market control premiums — and thereafter transfers every ratification outcome to the insider bloc at each shareholder vote, while moving growth-equity access outward to public investors.
% ABSENT_VOICES: Proxy advisers and investor coalitions object today but their objections terminate in a vote channel the structure itself controls — effectively excluded from any decision their arguments could change. Future purchasers at conversion events, sunset expirations, or recapitalizations hold no seat at the original consent moment. Index-committee deliberations over dual-class eligibility proceed without holder representation.
% DISAPPEARANCE_RATIONALE: If the consent-legitimation regime collapsed overnight — disclosure no longer accepted as discharging the legitimacy duty — founder-led firms would reroute: stay private longer, adopt capped-vote or mandatory-sunset architectures preemptively, or list in jurisdictions that already condition dual-class on sunsets. IPO composition, index weights, and underwriting fee pools would rearrange around the new legitimacy terms rather than continue unchanged.
% FOUNDING_PROBLEM: Founders taking companies public faced a tradeoff between raising external capital and keeping the voting control their long-horizon strategies required; the disclosure regime was built to let both sides resolve that tradeoff contractually — founders keep differentiated voting stock, investors receive full written terms and decide for themselves.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the founder seat: continued two-sided uptake documented in exchange listing statistics and registration filings (issuers keep electing the structure, buyers keep clearing it at disclosed terms), and the SEC's 2019 concept-roundtable record treating disclosure as a functioning mechanism. Partially disputed: proxy-adviser policy papers corroborate that the underlying problem exists while denying that disclosure alone settles the governance question — attesting the problem's persistence and contesting this reading's resolution of it.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__disclosure_consent, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__disclosure_consent, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__disclosure_consent, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dual_class_legitimacy__disclosure_consent, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__disclosure_consent, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__disclosure_consent_tests).
:- end_tests(dual_class_legitimacy__disclosure_consent_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.28 — low-to-moderate — because under this reading the arrangement's costs are consented and priced: the transfer is a voluntarily purchased governance discount, not a coerced levy. Suppression is low (0.16): genuine exit exists (decline to buy, sell, exclude the name from allocations), with residual friction only from index mandates and benchmark construction. Theater ratio 0.36: disclosure remains functional for institutional readers, but ratification votes decided by the insider bloc and boilerplate risk-factor drafting carry growing performative weight — hence the rising series, still below the Goodhart threshold. Accessibility collapse 0.30: one-share-one-vote issuers, exclusion screens, and private markets keep alternatives abundant. Resistance 0.58: sustained proxy-adviser opposition, sunset campaigns, and foreign sunset mandates meet a structure that nonetheless keeps clearing the market. The measurement series runs on one shared time grid (1984, 1992, 2000, 2009, 2017, 2024) so every tracked metric is authored at every examined point; no suppression_requirement series is authored because the enforcement picture is static — the SEC's administrative posture did not ratchet over the interval, and the contest moved through private ordering instead. Extractiveness declines through 2017 as disclosure standardizes and institutional pricing matures, then ticks back up as app-era retail flow and no-vote innovations stretch the consent premise; theater rises monotonically as ratification ritualizes. Claim and metrics are independently authored: both reflect this reading's lights, and the divergence the corpus measures is expected at the seat level, not the story level.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge structurally. The founder seat computes near the beneficiary pole: the arrangement subsidizes control retention at below-open-market cost, and every ratification is self-administered. Institutional Class A holders compute mildly above symmetry: constrained exit (mandates, tracking error, index membership) amplifies whatever uncompensated residue exists, which this reading prices as small but nonzero. Retail holders occupy the widest gap between the reading's premises and their situation — identical disclosure, radically different consumption capacity — and the directionality override moves that seat to 0.30 precisely because the derivation would otherwise read the declared beneficiary status as pure subsidy. Excluded governance bodies experience the enforcement layer as performance: their recommendations enter a tally the structure predetermines. Same listing, same prospectus, four different lived constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries: dual_class_founders (collector of the control differential) and both Class A holder groups (recipients of founder-led growth access — the coordination side of the ledger). No victim group is declared: under this reading no party bears uncompensated, suppressed harm; cost-bearing is consented and priced, which is exactly the reading's distinguishing claim against its minority_extraction sibling. The derivation therefore places all declared seats near the beneficiary end; the powerless-atom override corrects the one place the derivation is structurally blind — comprehension asymmetry between professional and retail readers of identical disclosure, the subject of the retail_consent_comprehension_gap omega. Suppression stays a raw structural input (unscaled); extractiveness is what the engine scales by directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading-indexed epsilon prevents two symmetrical misclassifications of this arrangement. Read as the minority_extraction sibling, the same structure is enforced asymmetric transfer — suppressed voice, extracted votes, identifiable victims. Read as the founder_stewardship sibling, it approaches pure coordination serving all holders. Fixed to the disclosure_consent referent, the story keeps both the genuine coordination function (capital formation under agreed written terms) and the bounded residual burden (diluted governance at constrained-exit seats) visible without collapsing either into the other. Mandatrophy is not triggered: the founding problem — matching control-seeking founders with informed capital — remains live and is corroborated by continued two-sided market uptake from sources outside the benefiting parties, so the founding_problem_status x disappearance_verdict pair shows no dead-problem mismatch. The consent_duration omega tracks the decay path along which this reading could migrate toward its extraction-flavored sibling: if registration-time consent stops binding across the security's life, the arrangement's legitimacy claim ages out of its founding moment while the structure persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_locator_dual_class_legitimacy,
    'Which legitimacy source governs dual-class structures — informed consent at disclosure (this reading), stewardship outcomes serving all holders (founder_stewardship), or proportional entitlement of capital at risk (minority_extraction) — and can any single framework synthesize the three?',
    'Cross-reading comparison within the kernel corpus: classify all sibling files and locate the structural disagreement at the legitimacy-source element rather than adjudicating merits; a waiver-style synthesis (entitlement except where informed consent waives it) would merge readings.',
    'Adopting a sibling reading rewrites the beneficiary/victim structure entirely: founder_stewardship removes the residual cost-bearing at public-holder seats; minority_extraction declares public holders entitled-but-subordinated and pushes per-seat computation toward enforced asymmetric transfer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_locator_dual_class_legitimacy, conceptual, 'Committer-frame locator: this story instantiates the disclosure_consent reading of the dual_class_legitimacy kernel; siblings are separate constraints.').

omega_variable(
    retail_consent_comprehension_gap,
    'Does S-1 disclosure produce genuinely informed consent for the marginal retail purchaser, or only formal consent that satisfies the regulatory record?',
    'Investor-comprehension surveys and trading-flow studies correlating demonstrated understanding of the voting differential with purchase behavior; natural experiments from simplified-disclosure pilots.',
    'If consent is largely formal at the retail seat, residual burden there rises sharply, pushing per-seat computation toward hybrid extraction and weakening this reading''s foundational axiom from within.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retail_consent_comprehension_gap, empirical, 'Whether the consent mechanism functions identically across reader sophistication levels.').

omega_variable(
    valuation_pricing_of_governance_disparity,
    'Is the governance disparity actually priced into Class A valuations, as the consent-for-compensation premise requires?',
    'Meta-analysis of dual-class valuation studies (discount/premium estimates, matched-pair comparisons), replicated across jurisdictions with mandatory sunsets versus without.',
    'If the disparity is unpriced, consent becomes uncompensated burden; the secondary axiom fails empirically and extraction estimates converge toward the minority_extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(valuation_pricing_of_governance_disparity, empirical, 'Empirical status of the pricing premise underlying consent-based legitimacy.').

omega_variable(
    consent_duration_across_security_life,
    'Does registration-time consent bind across the security''s life, including conversion events, sunset lapses, and recapitalizations the original purchasers never voted on?',
    'Track post-IPO charter amendments and holder turnover: if most current holders never faced the original disclosure decision, consent decays into inherited position.',
    'If consent decays, legitimacy requires periodic reconsent mechanisms — structurally converging this reading toward minority_extraction''s proportional entitlement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_duration_across_security_life, conceptual, 'Temporal scope of the consent on which this reading rests its legitimacy claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__disclosure_consent, 1984, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t1984, dual_class_legitimacy__disclosure_consent, theater_ratio, 1984, 0.18).
narrative_ontology:measurement_basis(dual_tr_t1984, observed).
narrative_ontology:measurement(dual_tr_t1992, dual_class_legitimacy__disclosure_consent, theater_ratio, 1992, 0.21).
narrative_ontology:measurement_basis(dual_tr_t1992, observed).
narrative_ontology:measurement(dual_tr_t2000, dual_class_legitimacy__disclosure_consent, theater_ratio, 2000, 0.26).
narrative_ontology:measurement_basis(dual_tr_t2000, observed).
narrative_ontology:measurement(dual_tr_t2009, dual_class_legitimacy__disclosure_consent, theater_ratio, 2009, 0.29).
narrative_ontology:measurement_basis(dual_tr_t2009, observed).
narrative_ontology:measurement(dual_tr_t2017, dual_class_legitimacy__disclosure_consent, theater_ratio, 2017, 0.34).
narrative_ontology:measurement_basis(dual_tr_t2017, observed).
narrative_ontology:measurement(dual_tr_t2024, dual_class_legitimacy__disclosure_consent, theater_ratio, 2024, 0.36).
narrative_ontology:measurement_basis(dual_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(dual_be_t1984, dual_class_legitimacy__disclosure_consent, base_extractiveness, 1984, 0.4).
narrative_ontology:measurement_basis(dual_be_t1984, observed).
narrative_ontology:measurement(dual_be_t1992, dual_class_legitimacy__disclosure_consent, base_extractiveness, 1992, 0.35).
narrative_ontology:measurement_basis(dual_be_t1992, observed).
narrative_ontology:measurement(dual_be_t2000, dual_class_legitimacy__disclosure_consent, base_extractiveness, 2000, 0.31).
narrative_ontology:measurement_basis(dual_be_t2000, observed).
narrative_ontology:measurement(dual_be_t2009, dual_class_legitimacy__disclosure_consent, base_extractiveness, 2009, 0.26).
narrative_ontology:measurement_basis(dual_be_t2009, observed).
narrative_ontology:measurement(dual_be_t2017, dual_class_legitimacy__disclosure_consent, base_extractiveness, 2017, 0.25).
narrative_ontology:measurement_basis(dual_be_t2017, observed).
narrative_ontology:measurement(dual_be_t2024, dual_class_legitimacy__disclosure_consent, base_extractiveness, 2024, 0.28).
narrative_ontology:measurement_basis(dual_be_t2024, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(dual_class_legitimacy__disclosure_consent, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__disclosure_consent, resource_allocation).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy__founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy__minority_extraction).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the kernel dual_class_legitimacy per the epsilon-invariance principle: the colloquial label 'is the dual-class structure legitimate?' covers three structurally distinct claims with different epsilon values, beneficiary/victim structures, and failure modes. This file carries the disclosure_consent claim (consent-based legitimacy, lowest extraction). dual_class_legitimacy__founder_stewardship carries the outcome-based claim (control serves all holders); dual_class_legitimacy__minority_extraction carries the entitlement-based claim (proportional governance owed regardless of consent). Upstream/downstream structure: this reading is procedural upstream — the disclosure machinery it endorses is the infrastructure through which stewardship defenses operate in public markets, and its erosion (unpriced disparity, formal-only consent) feeds directly into the minority_extraction reading's evidentiary base. Each member links the other two via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dual_class_legitimacy__disclosure_consent, powerless, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
