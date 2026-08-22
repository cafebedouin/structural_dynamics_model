% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__disclosure_consent
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Dual-Class Share Legitimacy via Disclosure Consent
 *   domain: corporate_governance/securities_law
 *
 * SUMMARY:
 *   This constraint models one reading of the dual-class legitimacy kernel:
 *   legitimacy rests on informed consent secured through Securities Act
 *   disclosure at IPO, not on proportional governance rights. Under this
 *   reading, founders legally and ethically acquire the right to unilateral
 *   control when they disclose the voting structure in the S-1 registration
 *   statement and public shareholders buy Class A shares with full knowledge
 *   of the governance asymmetry. This reading does NOT claim the structure is
 *   benign or efficient — it claims the legitimacy question is RESOLVED by
 *   the disclosure-consent mechanism, not by evaluating control
 *   concentration. Extraction is measured at a moderate-low level (0.38)
 *   because the reading treats the governance transfer as a voluntary
 *   contractual choice, not coerced taking; suppression is low (0.22) because
 *   shareholders retain exit options (selling shares) and the structure is
 *   transparent. The constraint is claimed as rope (genuine coordination
 *   solving the short-termism problem) while recognizing that measurement
 *   could show extractive operation — the claim/metric gap is intentional and
 *   reflects the reading's own framing, not an error.
 *
 * KEY AGENTS:
 *   - founders_with_super_voting_shares: control-retaining principals (powerful/arbitrage) — benefit from unilateral authority without capital exposure
 *   - class_a_public_shareholders: purchased-in-with-knowledge principals (organized/arbitrage) — receive economic participation but subordinate governance; can exit by selling
 *   - institutional_investors: analytical seats (powerful/mobile) — evaluate tradeoffs and vote with their capital allocation; many exclude dual-class, others price it
 *   - securities_regulators: enforcement seat (institutional/analytical) — oversee disclosure completeness, not governance substantiveness
 *   - minority_shareholders_without_disclosure: structurally absent (powerless/constrained) — secondary-market purchasers or unsophisticated investors not in the IPO consent moment
 *   - employee_shareholders: bundled-consent payers (moderate/constrained) — equity compensation without independent choice; constrained by employment contingency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__disclosure_consent, 0.38).
domain_priors:suppression_score(dual_class_legitimacy__disclosure_consent, 0.22).
domain_priors:theater_ratio(dual_class_legitimacy__disclosure_consent, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, extractiveness, 0.38).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__disclosure_consent, rope).
narrative_ontology:human_readable(dual_class_legitimacy__disclosure_consent, "Dual-Class Share Legitimacy via Disclosure Consent").
narrative_ontology:topic_domain(dual_class_legitimacy__disclosure_consent, "corporate_governance/securities_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__disclosure_consent, '37d01060-2c3a-487e-9578-402c080761c7').
narrative_ontology:cs_kernel_codification('37d01060-2c3a-487e-9578-402c080761c7', fixed_text).
narrative_ontology:cs_authority_grounding('37d01060-2c3a-487e-9578-402c080761c7', lineage).
narrative_ontology:cs_interpretation_layer_present('37d01060-2c3a-487e-9578-402c080761c7').
narrative_ontology:cs_reading_relation('37d01060-2c3a-487e-9578-402c080761c7', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('37d01060-2c3a-487e-9578-402c080761c7', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_axiom('37d01060-2c3a-487e-9578-402c080761c7', foundational, disclosure_satisfies_legitimacy_duty).
narrative_ontology:cs_axiom_status(disclosure_satisfies_legitimacy_duty, holdable).
narrative_ontology:cs_axiom_grounding('37d01060-2c3a-487e-9578-402c080761c7', disclosure_satisfies_legitimacy_duty, conventional).
narrative_ontology:cs_axiom('37d01060-2c3a-487e-9578-402c080761c7', secondary, informed_consent_via_s1_procedure).
narrative_ontology:cs_axiom_status(informed_consent_via_s1_procedure, holdable).
narrative_ontology:cs_axiom_grounding('37d01060-2c3a-487e-9578-402c080761c7', informed_consent_via_s1_procedure, conventional).
narrative_ontology:cs_reference_frame('37d01060-2c3a-487e-9578-402c080761c7', securities_act_disclosure_legitimacy).
narrative_ontology:cs_drift_state('37d01060-2c3a-487e-9578-402c080761c7', contemporary_governance_reform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('37d01060-2c3a-487e-9578-402c080761c7', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, founders_with_super_voting_shares).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, class_a_public_shareholders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, institutional_investors).
narrative_ontology:constraint_victim(dual_class_legitimacy__disclosure_consent, class_a_public_shareholders).
narrative_ontology:constraint_victim(dual_class_legitimacy__disclosure_consent, employee_shareholders).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__disclosure_consent, securities_act_disclosure_duty).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__disclosure_consent, caveat_emptor_principle).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__disclosure_consent, contractual_freedom_in_capitalization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain voting control through Class B shares carrying 10:1 or higher voting rights despite owning a minority of capital. They set strategic direction unilaterally. Their legitimacy claim under this reading rests on transparent disclosure at IPO: purchasers of Class A knew the voting structure before investing. They benefit by retaining unilateral control without proportional capital exposure and can exit by diversifying or stepping down while maintaining board representation and control.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, founders_with_super_voting_shares, agenda_setter,
    powerful, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__disclosure_consent, founders_with_super_voting_shares, beneficiary).

% Purchase Class A shares with full disclosure of the voting disparity in the S-1 registration statement. They receive economic rights (dividends, liquidation proceeds) proportional to capital invested, but governance rights are asymmetric — they cannot overturn founder strategic choices. They can exit by selling shares at market price. They benefit from professional founder management and brand continuity but accept governance subordination as part of the contractual deal.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, class_a_public_shareholders, payer,
    organized, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__disclosure_consent, class_a_public_shareholders, beneficiary).

% Large asset managers and pension funds evaluate the governance structure as part of investment decision-making. Many maintain explicit governance standards and may exclude dual-class from portfolios; others price the control premium into valuation and buy willingly. They have analytical capacity to assess the tradeoff and sufficient scale to influence governance through proxy voting and engagement. They can exit by selling or declining to participate in secondary offerings.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, institutional_investors, beneficiary,
    powerful, biographical, mobile, global).

% Enforce the disclosure duty under securities law and oversee proxy statements. Under this reading, they certify that disclosure is complete and material facts are stated; the regulatory duty is satisfied by transparency, not by mandating equal voting rights. They do not approve or disapprove the structure itself — only its disclosure. They can enforce disclosure requirements through comment letters and compliance monitoring.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, securities_regulators, observer,
    institutional, generational, analytical, national).

% Secondary market purchasers or gift recipients who acquired shares without reading the S-1 or who are unsophisticated investors unable to assess governance implications. They are structurally absent from the IPO consent moment and have no voice in the initial contractual choice. Under this reading, they are excluded from the legitimacy foundation even if harmed by governance disparity. They face constrained exit (selling into illiquid positions or accepting subordinate returns).
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, minority_shareholders_without_disclosure, excluded,
    powerless, biographical, constrained, global).

% Receive equity compensation (RSUs, options) in lieu of higher cash salary, often without independent choice of structure and with employment contingency on vesting. They acquire shares under the dual-class regime but their consent is bundled with employment, not freely elected. They have governance rights subordinate to founders but career dependence that constrains exit. Their ability to sell is limited by tax efficiency considerations and the employment relationship.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, employee_shareholders, payer,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__disclosure_consent, founders_with_super_voting_shares).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__disclosure_consent, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves founder vision and long-horizon strategy during public markets volatility: founders retain unilateral direction-setting authority, reducing board-capture risk, short-termism from activist investors, and dilution of founding mission through shareholder-driven pivots.
% TRANSFER_FUNCTION: Transfers governance authority and strategic control from public shareholders to founders, in exchange for capital and operational scale. Founders retain unilateral power; public shareholders receive economic participation (dividends, liquidation proceeds) and exit liquidity, but not governance voice proportional to capital.
% ABSENT_VOICES: Minority shareholders without disclosure (secondary-market purchasers, gift recipients, unsophisticated investors who did not read the S-1) are excluded from the IPO consent moment. Employees whose equity is compensation-bundled and not independently chosen are also structurally absent from the legitimacy claim. They would object that disclosure at IPO does not retroactively consent shareholders who acquired later or had no choice. They are not in the founding moment and have no seat in the contractual negotiation this reading rests on.
% DISAPPEARANCE_RATIONALE: If dual-class legitimacy via disclosure consent vanished, founders would lose their unilateral control mechanism. Public capital could only be raised via equal-voting IPOs, forcing a choice between dilution of control or foregoing public markets. Many founder-led companies would remain private or seek alternative capital structures. The constraint's disappearance would reshape IPO terms across technology and media sectors.
% FOUNDING_PROBLEM: Public markets pressurize founders toward quarterly earnings optimization and activist intervention, undermining long-horizon mission and operational independence. Founders need a mechanism to raise capital without surrendering strategic control.
% FOUNDING_PROBLEM_CORROBORATION: Founders and venture capital investors consistently cite activist pressure and short-termism as real harms they have experienced. Academic research (Bebchuk, Fried on private benefits; Edmans on long-termism) and public company case studies document instances of activist campaigns disrupting long-horizon strategy. However, counter-evidence is substantial: many founder-led companies (Ford, Berkshire Hathaway, Johnson & Johnson) maintained long-horizon strategy under equal-voting structures; institutional investors and governance scholars argue the empirical link between dual-class and better long-term outcomes is contested. The founding problem is live but the causal link between dual-class structures and mission preservation is actively disputed.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__disclosure_consent, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__disclosure_consent, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__disclosure_consent, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dual_class_legitimacy__disclosure_consent, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__disclosure_consent, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness starts at 0.32 and drifts gradually to 0.38 over the interval, reflecting two dynamics: (1) founder control becomes more entrenched and exercised as company matures (raising true extraction if founders pursue private benefits); (2) the reading's own logic treats this as priced-in contractual exchange, not extraction, so the metric measures what extractive operation looks like under this reading, not the reading's verdict on legitimacy. Suppression is low and stable (0.18–0.22) because the constraint operates via transparency and exit options, not force — shareholders who object can sell. Theater is minimal (0.10–0.15) because the governance asymmetry is the actual function, not a proxy for something else. Accessibility collapse is moderate (0.45) because alternatives DO exist (equal-voting IPOs, staying private, founder-led private companies) but the capital advantages of public markets make alternatives less attractive for high-growth founders. Resistance is moderate-high (0.52) because institutional investors, governance advocates, and minority shareholders mount real pressure to limit dual-class structures (sunset provisions, vote-dilution, regulatory proposals) — the constraint faces sustained challenge despite the reading's legitimacy claim.
 *
 * PERSPECTIVAL GAP:
 *   The founder seat and the public shareholder seat diverge sharply. From the founder position, the constraint is the legitimate price of capital access — transparency satisfies the duty, investors buy with eyes open, the reading is complete. From the public shareholder position, especially the minority without direct consent (secondary purchasers, employees), the constraint operates as governance subordination they did not actively choose; the reading's consent-at-IPO frame does not bind them retrospectively. Institutional investors sit between: they have analytical capacity and portfolio scale to evaluate the tradeoff, but also often adopt governance policies that prefer equal voting. The engine computes these divergences from the stakeholder power/exit/time_horizon combinations; the reading does not adjudicate across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Founders are the structural beneficiary: they retain unilateral control (d ≈ 0.1). Class A public shareholders sit near symmetric (d ≈ 0.5) — they receive economic participation and exit liquidity, but governance subordination; the reading frames this as a priced contractual choice, not extraction. Institutional investors have mobile exit (d ≈ 0.3, beneficiary-leaning because they can exclude dual-class from portfolios). Minority shareholders without disclosure face constrained exit and no consent-at-IPO protection (d ≈ 0.8, near target, but the reading excludes them from the legitimacy frame — they are absent voices). Employees have constrained exit and bundled consent (d ≈ 0.65). The directionality map reflects the reading's own framing: consent-at-IPO grants founders legitimacy; those outside the consent moment remain potential targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (short-termism pressure, activist capture) is LIVE but the empirical link between dual-class structures and mission preservation is CONTESTED. The reading does not depend on proving the structure actually works — it depends on the consent mechanism. If empirical evidence accumulated that dual-class founders also succumbed to short-termism, the founding problem would remain but the constraint's instrumental case would weaken. The mandatrophy risk is conditional: if founders demonstrate they use unilateral control to pursue private benefits (empire-building, related-party transactions, wasteful compensation) rather than long-horizon mission, the consent frame becomes harder to defend and the reading could shift from rope (genuine coordination with consent) toward snare (extraction defended by a cover story). The measurement series shows stable extraction (0.32–0.39) without sharp escalation, suggesting the constraint has not yet entered acute mandatrophy; but the theater ratio's slow rise (0.10→0.15) hints at increasing performative governance (founder control exercised but justified via mission rhetoric rather than demonstrated long-term outperformance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secondary_market_consent_binding,
    'Does IPO-time disclosure of the dual-class structure ethically or legally bind secondary-market shareholders who acquired shares after IPO without reading the S-1?',
    'Doctrinal analysis of securities law (does disclosure duty run to secondary purchasers?) combined with empirical data on secondary-market share distribution (what fraction of Class A is held by post-IPO purchasers?) and survey evidence on disclosure awareness among secondary shareholders.',
    'If secondary purchasers are NOT bound by IPO disclosure, the consent frame is narrower — legitimacy is secured only for the IPO cohort, not the full public shareholder base. This weakens the disclosure_consent reading and strengthens minority_extraction. If secondary purchasers ARE presumed to have constructive notice, the reading holds but its empirical claim becomes contestable (are secondary shareholders actually aware?).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secondary_market_consent_binding, empirical, 'Whether disclosure-at-IPO binds subsequent secondary-market shareholders.').

omega_variable(
    consent_bundled_with_employment,
    'When employees receive equity compensation in the form of dual-class stock, is the governance subordination part of a voluntary compensation bargain or an imposed term of employment?',
    'Analysis of employment contracts and RSU grant documents to determine whether employees can negotiate equity terms independently, and survey/interview data on employee awareness and acceptance of governance structure.',
    'If employee equity is truly bundled (take it or leave the job) and employees are unaware of governance implications, the consent frame fails for the employee population. This would narrow the legitimacy claim and expose employees as potentially non-consenting payers. If employees actively negotiate and price the governance subordination, the consent frame holds and extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_bundled_with_employment, empirical, 'Whether employee equity compensation includes genuine consent to governance terms.').

omega_variable(
    founder_control_versus_private_benefit,
    'Does founder exercise of unilateral control predominantly serve long-horizon mission (the founding_problem case) or predominantly serve founder private benefits (salaries, related-party transactions, empire-building)?',
    'Long-term empirical study comparing founder-controlled dual-class companies against founder-controlled equal-voting and professional-managed companies on: R&D investment, acquisition/divestment patterns, related-party transaction frequency, founder compensation growth, company longevity, shareholder returns.',
    'If private benefit extraction dominates, the founding_problem justification dissolves and the constraint shifts toward snare (extraction defended by short-termism rhetoric). If long-horizon mission dominates, the reading''s coordination claim strengthens and extraction remains moderate. If the pattern is mixed (some founders, some private benefit), the classification depends on the agent distribution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founder_control_versus_private_benefit, empirical, 'Whether founder control is exercised for mission alignment or private benefit extraction.').

omega_variable(
    institutional_investor_constraint_on_extraction,
    'Do institutional investor governance standards and capital allocation decisions effectively constrain founder extraction, or do they merely set governance guidelines that founders can ignore?',
    'Empirical study of institutional investor voting patterns on founder-compensation packages, related-party transactions, and proposals to eliminate dual-class; analysis of institutional capital flight from dual-class companies; comparison of founder private benefits across high-institutional vs. low-institutional ownership bases.',
    'If institutional investors effectively constrain extraction through governance engagement and capital discipline, the perceived extraction is lower and the rope classification holds. If institutional investors adopt governance standards but lack enforcement power, the constraint shows higher theater and the rope/snare boundary shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_investor_constraint_on_extraction, empirical, 'Whether institutional investor governance engagement constrains founder extraction.').

omega_variable(
    competing_legitimacy_readings_coexistence,
    'Can the disclosure_consent reading coexist with the founder_stewardship and minority_extraction readings in the same institutional and legal framework, or do they logically foreclose one another?',
    'Doctrinal and conceptual analysis of how courts, regulators, and governance bodies have treated the three readings simultaneously. Do they treat dual-class as procedurally legitimate (disclosure) AND substantively justified (stewardship) AND protecting minority rights (limiting founder extraction)? Or have different actors adopted incompatible positions?',
    'If all three readings can coexist, the constraint operates in a contested space and all three classification branches remain live. If readings foreclose one another, the constraint necessarily belongs to one family and the others are incoherent within the same framework. The engine''s cross-reading coupling analysis depends on this empirical fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_legitimacy_readings_coexistence, conceptual, 'Whether the three legitimacy readings of dual-class structures logically coexist or foreclose one another.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__disclosure_consent, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__disclosure_consent, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(dual_tr_t0, observed).
narrative_ontology:measurement(dual_tr_t5, dual_class_legitimacy__disclosure_consent, theater_ratio, 5, 0.11).
narrative_ontology:measurement_basis(dual_tr_t5, observed).
narrative_ontology:measurement(dual_tr_t10, dual_class_legitimacy__disclosure_consent, theater_ratio, 10, 0.13).
narrative_ontology:measurement_basis(dual_tr_t10, observed).
narrative_ontology:measurement(dual_tr_t15, dual_class_legitimacy__disclosure_consent, theater_ratio, 15, 0.14).
narrative_ontology:measurement_basis(dual_tr_t15, observed).
narrative_ontology:measurement(dual_tr_t20, dual_class_legitimacy__disclosure_consent, theater_ratio, 20, 0.15).
narrative_ontology:measurement_basis(dual_tr_t20, observed).
narrative_ontology:measurement(dual_tr_t25, dual_class_legitimacy__disclosure_consent, theater_ratio, 25, 0.15).
narrative_ontology:measurement_basis(dual_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__disclosure_consent, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(dual_be_t0, observed).
narrative_ontology:measurement(dual_be_t5, dual_class_legitimacy__disclosure_consent, base_extractiveness, 5, 0.35).
narrative_ontology:measurement_basis(dual_be_t5, observed).
narrative_ontology:measurement(dual_be_t10, dual_class_legitimacy__disclosure_consent, base_extractiveness, 10, 0.37).
narrative_ontology:measurement_basis(dual_be_t10, observed).
narrative_ontology:measurement(dual_be_t15, dual_class_legitimacy__disclosure_consent, base_extractiveness, 15, 0.38).
narrative_ontology:measurement_basis(dual_be_t15, observed).
narrative_ontology:measurement(dual_be_t20, dual_class_legitimacy__disclosure_consent, base_extractiveness, 20, 0.39).
narrative_ontology:measurement_basis(dual_be_t20, observed).
narrative_ontology:measurement(dual_be_t25, dual_class_legitimacy__disclosure_consent, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(dual_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__disclosure_consent, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(dual_su_t0, observed).
narrative_ontology:measurement(dual_su_t5, dual_class_legitimacy__disclosure_consent, suppression_requirement, 5, 0.19).
narrative_ontology:measurement_basis(dual_su_t5, observed).
narrative_ontology:measurement(dual_su_t10, dual_class_legitimacy__disclosure_consent, suppression_requirement, 10, 0.2).
narrative_ontology:measurement_basis(dual_su_t10, observed).
narrative_ontology:measurement(dual_su_t15, dual_class_legitimacy__disclosure_consent, suppression_requirement, 15, 0.21).
narrative_ontology:measurement_basis(dual_su_t15, observed).
narrative_ontology:measurement(dual_su_t20, dual_class_legitimacy__disclosure_consent, suppression_requirement, 20, 0.22).
narrative_ontology:measurement_basis(dual_su_t20, observed).
narrative_ontology:measurement(dual_su_t25, dual_class_legitimacy__disclosure_consent, suppression_requirement, 25, 0.22).
narrative_ontology:measurement_basis(dual_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__disclosure_consent, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dual_class_legitimacy__disclosure_consent, 0.12).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy__founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy__minority_extraction).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, securities_act_disclosure_duty).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, activist_investor_governance_intervention).

% DUAL FORMULATION NOTE:
% The dual_class_legitimacy kernel decomposes into three constraint stories, each instantiating a different reading of what legitimacy means in the context of concentrated founder voting. disclosure_consent treats legitimacy as procedurally satisfied by transparent disclosure at IPO; founder_stewardship treats legitimacy as grounded in demonstrated competence and long-horizon alignment; minority_extraction treats dual-class structures as inherently violating shareholder governance rights. The three stories share a referent (the standing dual-class arrangement) but instantiate incompatible legitimacy claims. Each story has its own extractiveness (ε), stakeholder structure, and classification; the engine computes per-reading verdicts and the corpus measures how readings contest the same institutional terrain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
