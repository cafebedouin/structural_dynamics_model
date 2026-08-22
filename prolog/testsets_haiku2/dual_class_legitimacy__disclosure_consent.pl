% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__disclosure_consent
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: dual_class_legitimacy__disclosure_consent
 *   human_readable: Dual-Class Stock Legitimacy via Disclosure and Informed Consent
 *   domain: corporate_governance/securities_law
 *
 * SUMMARY:
 *   Under the disclosure-consent reading, dual-class stock legitimacy rests
 *   entirely on Securities Act disclosure and informed investor consent.
 *   Founders disclose the governance disparity in the S-1; investors choose
 *   to buy Class B shares knowing their limited voting power; regulators
 *   enforce only the completeness and truthfulness of disclosure, not the
 *   fairness of the structure. The legitimacy claim is contractual: investors
 *   are sophisticated repeat players with perfect information; they price
 *   governance risk into valuation; the constraint is not extraction (because
 *   exit and repricing are available) but a negotiated governance term. This
 *   reading stands against two siblings: the founder-stewardship reading
 *   (which claims concentrated control serves all shareholders regardless of
 *   disclosure) and the minority-extraction reading (which claims governance
 *   disparity violates fiduciary duties to minorities regardless of initial
 *   consent).
 *
 * KEY AGENTS:
 *   - founders_with_super_voting_rights: Institutional actor, agenda-setter; retain control through super-voting shares; disclose structure; defend founder stewardship narrative.
 *   - minority_class_b_investors: Organized powerful actors, payers; buy Class B shares knowing governance disparity; price it and exit if misaligned; represent the consent substrate.
 *   - securities_regulators: Institutional observer and rule-setter; enforce disclosure adequacy; do not adjudicate governance fairness.
 *   - institutional_asset_managers: Powerful actors, dual position (beneficiaries of disclosure transparency, payers of governance risk); set ESG policies and capital allocation.
 *   - minority_shareholder_litigants: Powerless excluded actors; those who bought post-IPO and inherited the governance structure without initial consent.
 *   - competing_regulatory_regimes: Institutional observers; EU, Canada, Singapore enforce different governance standards.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__disclosure_consent, 0.31).
domain_priors:suppression_score(dual_class_legitimacy__disclosure_consent, 0.18).
domain_priors:theater_ratio(dual_class_legitimacy__disclosure_consent, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, extractiveness, 0.31).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__disclosure_consent, rope).
narrative_ontology:human_readable(dual_class_legitimacy__disclosure_consent, "Dual-Class Stock Legitimacy via Disclosure and Informed Consent").
narrative_ontology:topic_domain(dual_class_legitimacy__disclosure_consent, "corporate_governance/securities_law").

domain_priors:requires_active_enforcement(dual_class_legitimacy__disclosure_consent).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__disclosure_consent, '3b16bb64-aef9-4c0a-9663-9ccc56ceeb98').
narrative_ontology:cs_kernel_codification('3b16bb64-aef9-4c0a-9663-9ccc56ceeb98', formalized).
narrative_ontology:cs_authority_grounding('3b16bb64-aef9-4c0a-9663-9ccc56ceeb98', extraction).
narrative_ontology:cs_interpretation_layer_present('3b16bb64-aef9-4c0a-9663-9ccc56ceeb98').
narrative_ontology:cs_reading_relation('3b16bb64-aef9-4c0a-9663-9ccc56ceeb98', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('3b16bb64-aef9-4c0a-9663-9ccc56ceeb98', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_axiom('3b16bb64-aef9-4c0a-9663-9ccc56ceeb98', foundational, securities_disclosure_sufficient_legitimacy).
narrative_ontology:cs_axiom_status(securities_disclosure_sufficient_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('3b16bb64-aef9-4c0a-9663-9ccc56ceeb98', securities_disclosure_sufficient_legitimacy, deontological).
narrative_ontology:cs_axiom('3b16bb64-aef9-4c0a-9663-9ccc56ceeb98', secondary, investor_consent_waives_governance_parity_claim).
narrative_ontology:cs_axiom_status(investor_consent_waives_governance_parity_claim, holdable).
narrative_ontology:cs_axiom_grounding('3b16bb64-aef9-4c0a-9663-9ccc56ceeb98', investor_consent_waives_governance_parity_claim, instrumental).
narrative_ontology:cs_reference_frame('3b16bb64-aef9-4c0a-9663-9ccc56ceeb98', securities_act_disclosure_regime).
narrative_ontology:cs_drift_state('3b16bb64-aef9-4c0a-9663-9ccc56ceeb98', contemporary_esg_governance_escalation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3b16bb64-aef9-4c0a-9663-9ccc56ceeb98', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, founders_with_super_voting_rights).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, institutional_asset_managers).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, unaffiliated_board_members).
narrative_ontology:constraint_victim(dual_class_legitimacy__disclosure_consent, minority_class_b_investors).
narrative_ontology:constraint_victim(dual_class_legitimacy__disclosure_consent, institutional_asset_managers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain super-voting Class A shares (10 votes per share) while selling Class B shares (1 vote per share) to public investors. Set corporate strategy, elect the board, and control major decisions unilaterally. Disclose the governance structure in the S-1 prospectus and SEC filings. Justify super-voting as enabling long-horizon mission execution without quarterly pressure from public markets. Have full exit optionality: can hold indefinitely, sell shares, or spin off the company while maintaining control through super-voting.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, founders_with_super_voting_rights, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Buy Class B shares at IPO or in secondary markets, knowing the governance disparity is disclosed in the S-1 prospectus and ongoing SEC filings. They accept limited voting power (1 vote per share) in exchange for equity participation and liquidity. Their exit is available through liquid public markets: they can sell shares if the governance structure becomes intolerable or if the company underperforms. They bear the cost of founder control (strategic risk of founder error, misalignment between founder vision and shareholder returns) and price it into their investment decision via valuation multiples.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, minority_class_b_investors, payer,
    organized, biographical, mobile, global).

% Enforce Securities Act disclosure requirements (15 U.S.C. §77j): the issuer must truthfully and completely disclose the dual-class structure, voting mechanics, and governance risks in the prospectus. Regulators do not adjudicate the fairness or wisdom of the governance structure itself—that is a matter of contract and market pricing. Their authority is confined to procedural legitimacy: ensuring disclosure is complete, accurate, and enables informed investor choice. They can enforce disclosure through civil or administrative remedies; they can mandate additional disclosure if it is material.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, securities_regulators, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__disclosure_consent, securities_regulators, observer).

% Make portfolio decisions on Class B holdings based on their stated ESG and governance policies. Some asset managers (BlackRock, State Street, Vanguard) have published guidance on dual-class structures: some exclude them, others hold them as strategic bets on founder-led outperformance. They benefit from transparent disclosure (enabling their governance policies to have clear criteria) and from the governance disparity being priced into public markets (creating arbitrage opportunities for managers with different governance preferences). They are constrained in their exit by fiduciary duties to their own investors and by the size of their positions relative to market liquidity.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, institutional_asset_managers, beneficiary,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__disclosure_consent, institutional_asset_managers, payer).

% Serve on the board of the company but lack voting power to override founder decisions on major strategic matters. They provide counsel, fiduciary review (duty of care, duty of loyalty), and reputational credibility to investors. They benefit from legitimacy conferred by the disclosure regime (their presence signals governance seriousness to investors and reduces perceived founder-control risk). They are constrained by limited actual authority to change direction and by fiduciary duties that may conflict with founder preferences if a strategy appears value-destructive.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, unaffiliated_board_members, beneficiary,
    moderate, biographical, constrained, global).

% Minority shareholders who acquired Class B shares in secondary markets (post-IPO) or who inherited shares. They did not participate in the original IPO consent negotiation and did not have the opportunity to reject the dual-class structure at purchase. They may file derivative suits or shareholder class actions arguing breach of fiduciary duty, inadequate disclosure, or that governance disparity violates state corporate law. They are excluded from voting power and from initial governance negotiation; their exit is constrained by liquidity and transaction costs relative to their position size.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, post_ipo_minority_shareholder_litigants, excluded,
    powerless, biographical, constrained, national).

% Other jurisdictions (European Union, Canada, Singapore, Hong Kong) enforce different governance standards. The EU banned or heavily restricted dual-class structures post-2009; Canada permits them with sunset provisions (7-10 years); Singapore restricts voting differential to 2:1. They observe the US disclosure-consent regime as one regulatory approach among several. They do not directly participate in US dual-class constraints but their policies influence global capital flows, corporate venue selection (Delaware vs. Europe), and the competitive landscape for founders choosing where to take companies public.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, competing_regulatory_regimes, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__disclosure_consent, founders_with_super_voting_rights).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__disclosure_consent, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the founder-investor coordination problem by establishing a clear, disclosed governance contract: founders retain mission control; investors get transparency, liquidity, and priced-in governance risk. Eliminates the need for ongoing renegotiation of control by fixing it upfront and disclosing it completely.
% TRANSFER_FUNCTION: Transfers governance authority from proportional capital ownership to founder super-voting shares. In exchange, transfers liquidity access and market valuation to investors. The constraint moves control to founders and capital to investors, with disclosure as the legitimating mechanism.
% ABSENT_VOICES: Employees (who bear the strategic risk of founder control but do not hold shares), creditors (who bear seniority risk and have no governance seat despite bearing downside risk), and future minority shareholders (who inherit the disclosed governance but did not participate in the initial consent mechanism) are structurally absent from the initial contracting and cannot renegotiate.
% DISAPPEARANCE_RATIONALE: If the dual-class structure and its enforcement vanished overnight, founders would lose controlling votes; investors would gain proportional voting power; companies would face immediate proxy fights and governance restructuring. Founders justifying long-horizon missions would lose the mechanism that enables them; institutional investors with ESG policies would hold shares now barred from their portfolios under dual-class restrictions. Capital flows and strategic risk allocation would reorganize around one-share-one-vote norm.
% FOUNDING_PROBLEM: Founders of high-growth technology and innovation companies face a structural conflict: investors demand board representation and voting power proportional to capital contributed, but founders fear quarterly pressure and mission drift from short-term-focused shareholders. Early dual-class structures (Google 2004, Facebook 2012, Snap 2017) emerged as a solution: founders keep control; investors get liquidity and upside without governance authority. The founding problem is the misalignment between founder long-horizon incentives and public-market short-termism.
% FOUNDING_PROBLEM_CORROBORATION: Founders of dual-class companies (Mark Zuckerberg, Evan Spiegel, Sergey Brin) have publicly stated that founder control prevents short-termism and enables long-horizon bets on moonshot missions. Institutional investors and asset manager statements confirm they continue to price governance disparity as a component of due diligence and retain or exclude dual-class holdings based on their governance preferences. Securities regulators (SEC, including staff guidance post-2012 dual-class boom) do not challenge the founding problem itself, only the adequacy of disclosure. Academic research in organizational economics (Baker & Gompers, Grossman & Hart) validates the founder-control-vs-investor-pressure tradeoff as a structural problem. The founding problem is corroborated by independent economic analysis and longitudinal studies of company performance under founder control vs. public ownership.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__disclosure_consent, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__disclosure_consent, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__disclosure_consent, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dual_class_legitimacy__disclosure_consent, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__disclosure_consent, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-low (0.31) because the constraint is structured as a negotiated market contract with complete disclosure and available exit. The founders gain control authority, but investors gain transparent pricing and liquidity. The constraint is not a hidden extraction mechanism; it is a disclosed governance term that investors can reject by not buying or by selling. Theater ratio is low (0.22): the governance structure serves a real strategic function (protecting founder mission from quarterly pressure), not performative maintenance. Suppression is low (0.18) because investors have legitimate exit options (sell shares, vote with their feet, deploy ESG exclusion) and regulators enforce disclosure rather than mandate governance alignment. The measurement series show near-flat trajectories: extractiveness is stable because the disclosure regime and market pricing are mature; theater is stable because the strategic function remains constant; suppression is stable because the enforcement mechanism (disclosure + exit availability) is established. The flat metrics reflect a mature constraint in equilibrium, not a rising or degrading one.
 *
 * PERSPECTIVAL GAP:
 *   The founder seat perceives the constraint as enabling mission-driven governance; the Class B investor seat perceives it as a priced governance risk they voluntarily accept; the regulatory seat perceives it as a transparent disclosure requirement; the minority litigant seat perceives it as unfair governance without their consent. The engine computes these divergences from the power atoms (institutional vs. organized vs. powerless), exit options (arbitrage vs. mobile vs. constrained), and time horizons (civilizational vs. biographical). The disclosure-consent reading anchors legitimacy in the investor seat's perspective: informed, market-priced choice. The founder-stewardship reading anchors it in the founder seat's perspective (mission alignment). The minority-extraction reading anchors it in the litigant seat's perspective (excluded powerless actors). All three readings coexist in law and debate.
 *
 * DIRECTIONALITY LOGIC:
 *   Founders occupy the beneficiary seat (d near 0): they retain super-voting control without bearing proportional capital risk. Class B investors occupy a symmetric or near-symmetric seat (d near 0.5): they gain liquidity and upside pricing but accept governance risk; the tradeoff is transparent and their exit option is available. Institutional asset managers are near-symmetric: they can choose to hold or exclude based on their ESG policies, and the market prices their preference. Minority shareholder litigants occupy a target seat (d near 1.0): they inherit governance disparity without having participated in the initial consent negotiation. The directionality is stable across the interval because the structural relationships do not change: disclosure and pricing mechanisms remain constant.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does NOT meet the criteria for mandatrophy: the founding problem (founder control vs. investor pressure) remains live and actively contested. The Securities Act disclosure regime (founded ~1933, dual-class disclosure formalized ~2012) still serves its coordination function: it enables founders and investors to negotiate governance terms transparently. There is no case where the original mandate has been superseded by drift into pure theatrical maintenance. The constraint persists because the underlying coordination problem persists: founders need control certainty; investors need information and exit optionality. The disclosure-consent framing prevents mandatrophy by keeping the legitimacy mechanism contractual and market-responsive rather than institutional inertia. If extractiveness or theater ratio were to rise substantially while disclosure remained static, that would signal a shift toward mandatrophy or toward the minority-extraction reading (governance disparity justified by inertia rather than by consent). The current flat metrics do not show such drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disclosure_adequacy_empirical,
    'Is Securities Act disclosure sufficient to establish informed consent when most retail investors do not read S-1 prospectuses and delegated asset managers operate under opaque internal governance policies?',
    'Behavioral finance research on actual investor reading rates and decisions; audit of asset manager governance policies and voting records; post-IPO survey of Class B shareholders on governance knowledge.',
    'If disclosure is routinely unread or misunderstood, the legitimacy claim shifts from contractual consent to a regulatory fiction. The constraint would be reclassified toward snare (suppression via information asymmetry) or tangled_rope (disclosure preserves coordination even if unread, but extraction persists).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disclosure_adequacy_empirical, empirical, 'Whether disclosed governance structures are actually understood by the investor population.').

omega_variable(
    pricing_efficiency_of_governance_disparity,
    'Does the market price founder control as a governance risk discount, a strategic premium, or neither—i.e., is governance disparity fully reflected in Class B valuation?',
    'Event study comparing Class A and Class B stock price reactions to founder departure, strategic pivot, or governance failure; regression analysis of valuation multiples controlling for governance structure.',
    'If governance disparity is not priced (i.e., Class B trades at the same multiple as Class A despite lower voting power), the disclosure regime fails to establish true consent: investors are paying full price for partial control. The constraint would be reclassified toward snare or tangled_rope (extraction not reflected in pricing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pricing_efficiency_of_governance_disparity, empirical, 'Whether the market price of Class B shares reflects the governance risk appropriately.').

omega_variable(
    reading_kernel_coexistence,
    'Are the three readings of the dual-class legitimacy kernel genuinely coexistent, or does one reading logically foreclose another within a single coherent framework?',
    'Formal logic analysis of the foundational axioms: does disclosure-consent logically entail founder stewardship is not required? Does fiduciary duty to minorities logically contradict disclosure-consent? Or do the readings represent genuinely different frames that can coexist as long as the parties holding them are different?',
    'If the readings are genuinely coexistent, the constraint is correctly framed as rope under the disclosure-consent reading and the siblings are separate constraints with different ε values and stakeholders. If one reading forecloses another, the constraint may be single-frame-analyzed and the kernel is not genuinely contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_coexistence, conceptual, 'Whether the three dual-class legitimacy readings are logically coexistent or foreclosed by one another.').

omega_variable(
    consent_voluntariness_for_post_ipo_shareholders,
    'Is the disclosure-consent framing valid for shareholders who buy Class B shares years after the IPO, without having negotiated the dual-class terms at founding?',
    'Legal analysis of whether securities law treats post-IPO purchasers as having consented to inherited governance; survey of minority shareholder litigants on whether they felt they agreed to governance disparity.',
    'If post-IPO shareholders are not treated as having consented, the constraint operates as extraction on that cohort (they inherit governance disparity without choice). The constraint may need to be decomposed: one story for IPO-era consent (rope), another for post-IPO inherited governance (snare or tangled_rope). Or the consent reading is narrow and applies only to founding-era investors, making the broader constraint less rope-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_voluntariness_for_post_ipo_shareholders, conceptual, 'Whether the disclosure-consent reading is valid for shareholders who did not participate in the IPO.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__disclosure_consent, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__disclosure_consent, theater_ratio, 0, 0.15).
narrative_ontology:measurement(dual_tr_t5, dual_class_legitimacy__disclosure_consent, theater_ratio, 5, 0.18).
narrative_ontology:measurement(dual_tr_t10, dual_class_legitimacy__disclosure_consent, theater_ratio, 10, 0.2).
narrative_ontology:measurement(dual_tr_t15, dual_class_legitimacy__disclosure_consent, theater_ratio, 15, 0.22).
narrative_ontology:measurement(dual_tr_t20, dual_class_legitimacy__disclosure_consent, theater_ratio, 20, 0.22).
narrative_ontology:measurement(dual_tr_t25, dual_class_legitimacy__disclosure_consent, theater_ratio, 25, 0.22).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__disclosure_consent, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(dual_be_t5, dual_class_legitimacy__disclosure_consent, base_extractiveness, 5, 0.29).
narrative_ontology:measurement(dual_be_t10, dual_class_legitimacy__disclosure_consent, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(dual_be_t15, dual_class_legitimacy__disclosure_consent, base_extractiveness, 15, 0.31).
narrative_ontology:measurement(dual_be_t20, dual_class_legitimacy__disclosure_consent, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(dual_be_t25, dual_class_legitimacy__disclosure_consent, base_extractiveness, 25, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__disclosure_consent, suppression_requirement, 0, 0.16).
narrative_ontology:measurement(dual_su_t5, dual_class_legitimacy__disclosure_consent, suppression_requirement, 5, 0.17).
narrative_ontology:measurement(dual_su_t10, dual_class_legitimacy__disclosure_consent, suppression_requirement, 10, 0.18).
narrative_ontology:measurement(dual_su_t15, dual_class_legitimacy__disclosure_consent, suppression_requirement, 15, 0.18).
narrative_ontology:measurement(dual_su_t20, dual_class_legitimacy__disclosure_consent, suppression_requirement, 20, 0.18).
narrative_ontology:measurement(dual_su_t25, dual_class_legitimacy__disclosure_consent, suppression_requirement, 25, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__disclosure_consent, resource_allocation).
narrative_ontology:boltzmann_floor_override(dual_class_legitimacy__disclosure_consent, 0.12).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy__founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy__minority_extraction).

% DUAL FORMULATION NOTE:
% The 'dual_class_legitimacy' kernel is instantiated by three structurally distinct constraints, each with its own ε, stakeholders, and classification. Disclosure-consent (this story) grounds legitimacy in regulatory disclosure and market consent; ε=0.31 (moderate extractiveness offset by transparency and exit). Founder-stewardship grounds it in alignment incentives; expected higher ε due to governance disparity without explicit extraction mechanism. Minority-extraction grounds it in fiduciary breach and voting rights violation; expected highest ε (extraction without consent). All three readings share the referent (the 10-to-1 voting disparity) but diverge on what legitimates or delegitimizes it. Linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
