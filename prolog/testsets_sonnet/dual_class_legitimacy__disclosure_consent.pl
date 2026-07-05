% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__disclosure_consent
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: dual_class_legitimacy__disclosure_consent
 *   human_readable: Dual-Class Share Legitimacy via Disclosure-Based Consent
 *   domain: corporate_governance/securities_law
 *
 * SUMMARY:
 *   This story instantiates the disclosure_consent reading of the dual-class
 *   share legitimacy kernel: the claim that a founder's retention of
 *   super-voting control over public shareholders is legitimated by
 *   Securities Act disclosure rather than by any requirement of control
 *   parity. Under this reading, the S-1 fully discloses the voting structure,
 *   sophisticated and retail investors alike price the governance disparity
 *   into the Class A offering price and subsequent trading price, and the
 *   arrangement is best understood as a contractual choice between
 *   control-seeking founders and control-indifferent capital, mediated by
 *   regulatory disclosure duties rather than by substantive governance
 *   mandates. This is deliberately narrower than either sibling reading: it
 *   does not claim the arrangement serves all shareholders through superior
 *   stewardship (that is founder_stewardship), and it does not claim minority
 *   shareholders are structurally owed proportional governance (that is
 *   minority_extraction). Those are separate constraints with separate ε
 *   values and separate stakeholder structures, linked here only by shared
 *   kernel membership.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__disclosure_consent, 0.38).
domain_priors:suppression_score(dual_class_legitimacy__disclosure_consent, 0.22).
domain_priors:theater_ratio(dual_class_legitimacy__disclosure_consent, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, extractiveness, 0.38).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__disclosure_consent, rope).
narrative_ontology:human_readable(dual_class_legitimacy__disclosure_consent, "Dual-Class Share Legitimacy via Disclosure-Based Consent").
narrative_ontology:topic_domain(dual_class_legitimacy__disclosure_consent, "corporate_governance/securities_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__disclosure_consent, '90dd95ec-3793-4bde-af63-efc9ae512d1a').
narrative_ontology:cs_kernel_codification('90dd95ec-3793-4bde-af63-efc9ae512d1a', formalized).
narrative_ontology:cs_authority_grounding('90dd95ec-3793-4bde-af63-efc9ae512d1a', expertise).
narrative_ontology:cs_interpretation_layer_present('90dd95ec-3793-4bde-af63-efc9ae512d1a').
narrative_ontology:cs_reading_relation('90dd95ec-3793-4bde-af63-efc9ae512d1a', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('90dd95ec-3793-4bde-af63-efc9ae512d1a', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_axiom('90dd95ec-3793-4bde-af63-efc9ae512d1a', foundational, disclosure_satisfies_legitimating_duty).
narrative_ontology:cs_axiom_status(disclosure_satisfies_legitimating_duty, holdable).
narrative_ontology:cs_axiom_grounding('90dd95ec-3793-4bde-af63-efc9ae512d1a', disclosure_satisfies_legitimating_duty, conventional).
narrative_ontology:cs_axiom('90dd95ec-3793-4bde-af63-efc9ae512d1a', foundational, governance_terms_are_price_discoverable).
narrative_ontology:cs_axiom_status(governance_terms_are_price_discoverable, holdable).
narrative_ontology:cs_axiom_grounding('90dd95ec-3793-4bde-af63-efc9ae512d1a', governance_terms_are_price_discoverable, empirically_contingent).
narrative_ontology:cs_reference_frame('90dd95ec-3793-4bde-af63-efc9ae512d1a', mandatory_disclosure_regime_1933_act).
narrative_ontology:cs_drift_state('90dd95ec-3793-4bde-af63-efc9ae512d1a', contemporary_index_ownership_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('90dd95ec-3793-4bde-af63-efc9ae512d1a', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, founder_control_group).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, underwriting_banks).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, class_a_investors_seeking_growth_exposure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dual_class_legitimacy__disclosure_consent, class_a_investors_seeking_growth_exposure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds Class B super-voting shares, retains board and strategic control despite owning a minority of total equity. Drafted the S-1 disclosure describing the voting structure in full before the IPO. Benefits from insulation against activist pressure and short-term market discipline; their exit options (control retention, sale of control blocks, or conversion triggers) are entirely theirs to exercise.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, founder_control_group, beneficiary,
    institutional, civilizational, arbitrage, national).

% Purchase Class A shares on public markets after reviewing SEC-mandated disclosure of the dual-class structure. Accept diminished per-share voting power in exchange for equity upside and liquidity. Can sell at any time at the market-clearing price, which reflects the disclosed governance disparity; no lock-in beyond ordinary market risk.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, class_a_investors_seeking_growth_exposure, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__disclosure_consent, class_a_investors_seeking_growth_exposure, payer).

% Structure and market the offering, certify disclosure adequacy through due diligence, and collect underwriting fees regardless of long-run governance outcomes. Their exposure to the constraint ends at settlement.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, underwriting_banks, beneficiary,
    institutional, immediate, arbitrage, global).

% Administer the disclosure regime that renders the dual-class structure lawful: mandates full risk-factor and voting-structure disclosure in the S-1, but does not require voting parity as a listing condition. Could impose one-share-one-vote listing rules but has chosen disclosure-based regulation as the governing standard for decades.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, sec_and_stock_exchange_listing_committees, agenda_setter,
    institutional, generational, analytical, national).

% Investors who read the disclosure and choose not to buy in are not part of the constraint at all — their exit is exercised before entry. They are mentioned here only to establish that the disclosure regime's consent claim depends on this population actually existing and being able to decline; if disclosure were pro forma rather than genuinely informative, this group's absence would be evidence of failure, not proof of consent.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, prospective_class_a_investors_who_decline, excluded,
    powerless, immediate, mobile, national).

% Study whether disclosure-based consent is doing the legitimating work claimed for it — whether investors actually price governance disparity accurately, or whether index-fund passive flows purchase Class A shares without any real assessment of the voting structure at all.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, securities_law_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows founders to raise public capital while retaining strategic control, and allows investors to obtain liquid equity exposure to a company's economics without needing (or wanting) operational control — a division of labor between capital-provision and management that many investors affirmatively prefer.
% TRANSFER_FUNCTION: Moves control rights away from the pro-rata capital-weighted default and concentrates them in the founder class; in exchange, that concentration is disclosed and, per this reading, priced into the Class A offering price at IPO and in every subsequent secondary trade.
% ABSENT_VOICES: Future minority shareholders who acquire shares post-IPO via index inclusion (rather than active choice) never individually evaluated the disclosure — they inherit the structure through passive fund flows. Employees compensated in Class A equity likewise did not choose the structure as investors; they received it as compensation.
% DISAPPEARANCE_RATIONALE: If dual-class structures were banned outright, some companies would not have gone public at all (founders citing loss of control as a dealbreaker), others would convert to single-class at a valuation discount or premium depending on market view of the concentration, and the disclosure apparatus itself (S-1 risk-factor drafting practice) would need no revision — this reading holds the disclosure regime, not the dual-class structure itself, as the object of legitimacy, so removing dual-class shares would not touch the underlying claim that disclosure-based consent is sufficient legitimation for any governance term investors can price.
% FOUNDING_PROBLEM: Public capital markets need a mechanism by which investors can rely on adequate information to price securities without needing to negotiate bespoke governance terms with each issuer — the Securities Act's disclosure regime substitutes ex ante mandated disclosure for ex post substantive governance mandates.
% FOUNDING_PROBLEM_CORROBORATION: Independent securities law scholarship (outside both founder and underwriter interests) generally affirms disclosure-based regulation as the operative U.S. federal securities law framework since 1933; however, the same scholarship is split on whether disclosure functions as genuine informed consent for governance terms specifically, as opposed to financial risk factors generally — see debates over rational apathy among diversified index investors who do not individually read S-1 filings.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__disclosure_consent, contested).
narrative_ontology:founding_problem_status(dual_class_legitimacy__disclosure_consent, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__disclosure_consent, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dual_class_legitimacy__disclosure_consent, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__disclosure_consent, 0.38, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored moderate-low (0.38) because under this reading's own premises, the governance disparity is priced ex ante — a rational Class A buyer who values control receives a lower offering price or discounted secondary price to compensate, so the 'extraction' is compensated rather than uncompensated transfer. It is not authored near zero because the reading itself concedes an omega: whether pricing is actually efficient for governance terms specifically (as opposed to financial terms generally) is contested, and passive index flows complicate the 'informed' half of informed consent. Suppression is low (0.22): no coercion compels anyone to buy Class A shares, and secondary market liquidity provides ongoing exit. Theater ratio is modest (0.28) reflecting that disclosure documents are lengthy and some risk-factor boilerplate is genuinely non-informative, but the core disclosure of voting structure is typically prominent and substantive, not merely decorative.
 *
 * PERSPECTIVAL GAP:
 *   The regulator seat (SEC/exchanges) experiences this as a working coordination mechanism — disclosure regime functioning as designed, no listing-standard intervention needed. The founder seat experiences it as a durable, arbitrage-grade entitlement. The Class A investor seat's experience is genuinely contested even within this reading: an investor who read the S-1 and consciously accepted the tradeoff experiences informed consent; an investor who acquired shares via index fund inclusion never made that individual assessment, which is why absent_voices names index-driven acquisition as the structural gap in the consent story even under this reading's own terms.
 *
 * DIRECTIONALITY LOGIC:
 *   Founders and underwriters are structural beneficiaries under this reading — founders retain control, underwriters collect fees regardless of governance outcome. Class A investors are coded as beneficiary/payer dual-role: they pay for reduced control rights but receive compensating consideration (this reading asserts the compensation is adequate; the sibling minority_extraction reading disputes this). No party is coded as a pure victim under this reading, because the disclosure_consent premise is precisely that no one is extracted from without compensating consent and price adjustment — that is what makes this reading distinct from minority_extraction, where victims are named explicitly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mandated disclosure substituting for negotiated governance terms) remains live — the Securities Act disclosure regime is neither obsolete nor superseded; issuers still must file S-1s and dual-class structures still must be disclosed. This forecloses a mandatrophy reading under this reading's own premises: there is no zombie mandate here, because the disclosure requirement continues to do genuine work (informing pricing) rather than persisting as inert ritual. Whether disclosure does ENOUGH work to fully legitimate control disparity is a different, harder question — routed to omega rather than resolved here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disclosure_consent_kernel_framing,
    'Is disclosure-based consent (this reading) the correct legitimating frame for dual-class structures, or do the sibling readings (founder_stewardship, minority_extraction) better capture the structural reality?',
    'This is a committer-level question, not resolvable within a single story. Each reading is authored as a separate constraint with its own ε and stakeholder structure; the kernel contest is tracked via cs_structure.reading_relations across the family, not resolved by evidence internal to any one reading.',
    'If the founder_stewardship reading is judged correct, this constraint''s coordination function is understated (real stewardship value exists beyond mere contractual choice). If minority_extraction is judged correct, this constraint''s extractiveness is substantially understated because disclosure does not actually price governance terms efficiently for diversified/passive holders.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(disclosure_consent_kernel_framing, conceptual, 'Which of the three kernel readings best describes actual dual-class governance legitimacy.').

omega_variable(
    efficient_pricing_of_governance_terms,
    'Do public markets actually price the value of voting control accurately into Class A share prices, or is governance disparity a term that rational-apathy and passive-index dynamics prevent from being efficiently priced (unlike, say, financial leverage or dividend policy)?',
    'Event studies comparing valuation multiples of dual-class vs. single-class IPOs controlling for sector/growth/founder-quality, and comparison of pricing efficiency for governance terms vs. financial terms specifically; examination of whether dual-class discount/premium narrows or widens as passive ownership share increases.',
    'If governance terms are shown to be efficiently priced, this reading''s extractiveness estimate (0.38) is well-supported. If passive-flow dynamics are shown to prevent efficient pricing of voting rights specifically, the ''informed consent'' premise weakens substantially even on this reading''s own terms, and effective extraction should be revised upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficient_pricing_of_governance_terms, empirical, 'Whether market pricing genuinely compensates for dual-class governance disparity.').

omega_variable(
    passive_index_consent_gap,
    'Can ''informed consent'' be attributed to shareholders who acquire Class A shares via passive index-fund inclusion rather than individual investment decision?',
    'Survey/behavioral data on whether retail beneficiaries of index funds are aware of, or would object to, dual-class structures within their holdings; examination of index-provider policies on dual-class share eligibility (e.g., S&P and FTSE exclusion rules adopted post-2017).',
    'If passive-flow shareholders cannot meaningfully be said to have consented, the disclosure_consent reading''s legitimating claim applies only to the shrinking population of active, informed IPO-stage buyers, and a growing share of actual Class A ownership falls outside this reading''s own consent framework — pushing structurally toward the minority_extraction reading for that ownership segment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(passive_index_consent_gap, conceptual, 'Whether passive/index ownership undermines the consent premise this reading depends on.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__disclosure_consent, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__disclosure_consent, theater_ratio, 0, 0.2).
narrative_ontology:measurement(dual_tr_t4, dual_class_legitimacy__disclosure_consent, theater_ratio, 4, 0.22).
narrative_ontology:measurement(dual_tr_t8, dual_class_legitimacy__disclosure_consent, theater_ratio, 8, 0.24).
narrative_ontology:measurement(dual_tr_t12, dual_class_legitimacy__disclosure_consent, theater_ratio, 12, 0.25).
narrative_ontology:measurement(dual_tr_t16, dual_class_legitimacy__disclosure_consent, theater_ratio, 16, 0.27).
narrative_ontology:measurement(dual_tr_t20, dual_class_legitimacy__disclosure_consent, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__disclosure_consent, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(dual_be_t4, dual_class_legitimacy__disclosure_consent, base_extractiveness, 4, 0.32).
narrative_ontology:measurement(dual_be_t8, dual_class_legitimacy__disclosure_consent, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(dual_be_t12, dual_class_legitimacy__disclosure_consent, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(dual_be_t16, dual_class_legitimacy__disclosure_consent, base_extractiveness, 16, 0.37).
narrative_ontology:measurement(dual_be_t20, dual_class_legitimacy__disclosure_consent, base_extractiveness, 20, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(dual_class_legitimacy__disclosure_consent, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__disclosure_consent, resource_allocation).
narrative_ontology:boltzmann_floor_override(dual_class_legitimacy__disclosure_consent, 0.15).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy__founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy__minority_extraction).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the dual_class_legitimacy kernel (see cs_structure.reading_relations). founder_stewardship claims concentrated control produces superior stewardship outcomes for all shareholders (coordination framing, likely rope or tangled_rope). minority_extraction claims minority shareholders are structurally entitled to proportional governance and its absence is uncompensated extraction (victim-centered framing, likely snare or tangled_rope). This story (disclosure_consent) claims the arrangement is legitimated by regulatory disclosure and priced consent (contractual-choice framing, authored here as rope with moderate-low extractiveness). All three share the same underlying corporate structure (a dual-class public company) but instantiate structurally distinct constraints with different ε values, different beneficiary/victim sets, and different classifications — per the ε-invariance principle, they are not the same constraint viewed three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
