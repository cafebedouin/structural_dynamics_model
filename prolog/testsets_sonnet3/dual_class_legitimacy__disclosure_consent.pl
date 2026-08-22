% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__disclosure_consent
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: dual_class_legitimacy__disclosure_consent
 *   human_readable: Dual-Class Share Structure Legitimated by S-1 Disclosure and Informed Purchase
 *   domain: corporate_governance/securities_law
 *
 * SUMMARY:
 *   This story instantiates the disclosure_consent reading of the
 *   dual_class_legitimacy kernel: the position that a dual-class share
 *   structure's legitimacy is fully established by adequate Securities Act
 *   disclosure at offering (and ongoing periodic disclosure thereafter), and
 *   by the voluntary purchase decisions of investors who had access to that
 *   disclosure. Under this reading, the fact that Class A holders have fewer
 *   votes per share than Class B/founder holders is not itself a legitimacy
 *   defect — it is a disclosed, priced term of the security, structurally
 *   analogous to disclosed preferred-stock terms or disclosed debt covenants.
 *   This reading does NOT assert that dual-class structures are either pure
 *   coordination (the founder_stewardship reading) or pure extraction (the
 *   minority_extraction reading); it asserts a third, narrower claim — that
 *   the legitimacy question is answered by the disclosure regime and is
 *   complete once disclosure is adequate, independent of whether the
 *   resulting allocation is efficient or fair on some other axis.
 *
 * KEY AGENTS:
 *   - founder_controlling_shareholders: primary beneficiary (institutional/arbitrage) — retains disclosed control premium
 *   - class_a_investors: nominal payer/beneficiary (organized/mobile) — purchases with disclosed terms, retains liquidity
 *   - underwriting_banks: beneficiary/agenda_setter (institutional/arbitrage) — certifies disclosure adequacy, collects fees
 *   - sec_disclosure_regime: agenda_setter (institutional/analytical) — administers the disclosure standard this reading treats as dispositive
 *   - index_fund_managers: excluded (organized/constrained) — mandate-compelled purchase outside discretionary consent framework
 *   - corporate_governance_scholars: observer (analytical) — assesses whether disclosed consent is meaningful consent
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
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__disclosure_consent, rope).
narrative_ontology:human_readable(dual_class_legitimacy__disclosure_consent, "Dual-Class Share Structure Legitimated by S-1 Disclosure and Informed Purchase").
narrative_ontology:topic_domain(dual_class_legitimacy__disclosure_consent, "corporate_governance/securities_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__disclosure_consent, 'ada127be-4976-4279-a198-e9ebc630bc17').
narrative_ontology:cs_kernel_codification('ada127be-4976-4279-a198-e9ebc630bc17', formalized).
narrative_ontology:cs_authority_grounding('ada127be-4976-4279-a198-e9ebc630bc17', expertise).
narrative_ontology:cs_interpretation_layer_present('ada127be-4976-4279-a198-e9ebc630bc17').
narrative_ontology:cs_reading_relation('ada127be-4976-4279-a198-e9ebc630bc17', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('ada127be-4976-4279-a198-e9ebc630bc17', dual_class_legitimacy__minority_extraction, influences).
narrative_ontology:cs_axiom('ada127be-4976-4279-a198-e9ebc630bc17', foundational, disclosure_adequacy_constitutes_legitimacy).
narrative_ontology:cs_axiom_status(disclosure_adequacy_constitutes_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('ada127be-4976-4279-a198-e9ebc630bc17', disclosure_adequacy_constitutes_legitimacy, conventional).
narrative_ontology:cs_axiom('ada127be-4976-4279-a198-e9ebc630bc17', secondary, governance_terms_are_priced_not_owed).
narrative_ontology:cs_axiom_status(governance_terms_are_priced_not_owed, holdable).
narrative_ontology:cs_axiom_grounding('ada127be-4976-4279-a198-e9ebc630bc17', governance_terms_are_priced_not_owed, instrumental).
narrative_ontology:cs_reference_frame('ada127be-4976-4279-a198-e9ebc630bc17', disclosure_based_procedural_legitimacy).
narrative_ontology:cs_drift_state('ada127be-4976-4279-a198-e9ebc630bc17', post_2017_index_exclusion_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ada127be-4976-4279-a198-e9ebc630bc17', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, founder_controlling_shareholders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, underwriting_banks).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, class_a_investors_seeking_price_discount).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, class_a_investors).
narrative_ontology:constraint_victim(dual_class_legitimacy__disclosure_consent, class_a_investors).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__disclosure_consent, informed_consent_doctrine).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__disclosure_consent, efficient_market_pricing_of_control_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain supervoting Class B (or equivalent) shares disclosed in the S-1 and proxy materials. Under this reading, their control premium is a bargained-for, disclosed feature of the security, not a defect requiring remedy — Class A purchasers priced it in at IPO and at every subsequent trade.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, founder_controlling_shareholders, beneficiary,
    institutional, generational, arbitrage, national).

% Purchase Class A shares in public markets with full access to S-1/S-3 disclosure describing the voting differential, sunset provisions (if any), and related-party governance terms. Under this reading they are not extracted from: they receive a market-clearing price that reflects the reduced voting rights, and they retain full liquidity to sell at any time. Their consent is evidenced by the transaction itself, made with disclosed information.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, class_a_investors, payer,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__disclosure_consent, class_a_investors, beneficiary).

% Structure and price the offering, draft the disclosure language satisfying Securities Act requirements, and collect underwriting fees regardless of share class structure. Their institutional role is to certify that disclosure is adequate for legal consent to attach.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, underwriting_banks, beneficiary,
    institutional, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__disclosure_consent, underwriting_banks, agenda_setter).

% Administers the registration and periodic-disclosure requirements (S-1, proxy rules, Item 402/403) that this reading treats as the entire legitimacy test: if the structure and its risks are disclosed per statute, the SEC's regulatory duty toward investor protection is discharged. Does not evaluate control parity as a separate legal question.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, sec_disclosure_regime, agenda_setter,
    institutional, generational, analytical, national).

% Must hold Class A shares of index constituents regardless of governance terms to track benchmarks, meaning their 'consent' is structurally compelled by fiduciary mandate rather than freely chosen — a fact this reading treats as outside the disclosure-consent framework, since index inclusion rules and mandate design are not securities-disclosure questions.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, index_fund_managers, excluded,
    organized, generational, constrained, global).

% Study whether disclosed-but-unequal control terms satisfy a meaningful notion of consent when index mandates, network effects, and diversification requirements narrow the practical alternative of not buying. Their analysis feeds the sibling readings without altering this reading's own internal standard.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, corporate_governance_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__disclosure_consent, founder_controlling_shareholders).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__disclosure_consent, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables founders to raise public capital while retaining strategic control, and enables investors to price and purchase a security whose terms — including governance rights — are transparently disclosed before purchase; the coordination problem solved is capital formation under asymmetric information, resolved by mandatory disclosure rather than mandatory equal voting.
% TRANSFER_FUNCTION: No transfer is asserted under this reading: the security's price already reflects the voting differential, so Class A holders are not paying more than the disclosed, control-adjusted value of what they buy. What moves is information (via the S-1) and capital (via the offering), not an unpriced governance premium.
% ABSENT_VOICES: Index fund managers and other mandate-constrained investors, whose participation is not fully discretionary and whose 'informed consent' is legally imputed rather than practically exercised, are not heard as a distinct category within securities disclosure doctrine — the doctrine treats all public purchasers uniformly as consenting parties once disclosure is made.
% DISAPPEARANCE_RATIONALE: If the disclosure-consent framework were abandoned in favor of mandatory control parity, dual-class issuances would either cease or convert, founders would lose a capital-raising option they currently value, and Class A pricing would shift upward to reflect newly-equal voting rights — a real rearrangement. But proponents of this reading argue the world is already 'unchanged' in the relevant sense: disclosure-satisfied transactions are not defective and nothing about them needs to rearrange; only the alternative frameworks claim rearrangement is needed.
% FOUNDING_PROBLEM: Public capital markets need a mechanism by which issuers with heterogeneous governance preferences (including founder control) can raise capital from dispersed investors without either side being deceived about what is being sold — the Securities Act's disclosure regime was built to solve information asymmetry, not to standardize governance terms.
% FOUNDING_PROBLEM_CORROBORATION: SEC rulemaking history and federal securities case law (e.g., the 'total mix of information' standard from TSC Industries v. Northway) attest, from outside the founder/underwriter beneficiary set, that disclosure adequacy — not substantive fairness of terms — has been the operative legal standard since the 1930s Acts; this is a judicial and regulatory attestation, not a self-serving claim by controlling shareholders.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__disclosure_consent, contested).
narrative_ontology:founding_problem_status(dual_class_legitimacy__disclosure_consent, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__disclosure_consent, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored moderate-low (0.38 at interval end) rather than negligible, because even under this reading's own terms, disclosure does not fully eliminate an information-and-power asymmetry: retail Class A purchasers rarely read full S-1 risk factor sections, and post-IPO share issuances can dilute voting power in ways not fully anticipated at initial purchase. Suppression is low (0.22) because no coercive mechanism forces anyone to buy Class A shares — the mechanism is disclosure plus market choice, not enforcement. Theater ratio is modest (0.28) reflecting that boilerplate risk-factor language in S-1 filings has some performative quality (satisfying the letter of disclosure law without necessarily producing genuine investor comprehension) without being predominantly theatrical. Accessibility collapse is moderate (0.35): once purchased, exit via sale is easy (liquid public market), but exit from the governance term itself (converting Class A to equal voting) is not available to an individual holder.
 *
 * PERSPECTIVAL GAP:
 *   The founder/underwriter seat and the SEC seat should compute close to genuine coordination (rope-like): a real information-asymmetry problem is solved by mandated, standardized disclosure, and the resulting purchase decisions are treated as legally sufficient consent. The excluded index-fund seat, if given voice, would compute the same structure very differently — as a structure whose disclosure satisfies a legal test without producing anything resembling voluntary choice for compelled buyers. This gap is exactly the seam the sibling reading (minority_extraction) is built to name; this story deliberately does not resolve that gap, per Rule 1 — it authors only the disclosure_consent reading cleanly.
 *
 * DIRECTIONALITY LOGIC:
 *   Founders and underwriters sit near the beneficiary end of directionality: they retain or extract value from a disclosed, bargained structure. Class A investors are authored close to symmetric rather than as victims — this reading's central claim is that the purchase price already internalizes the governance disparity, so no uncompensated extraction occurs for a voluntary, informed buyer. This is why base_properties.victims is empty: under this reading's own lights there is no victim, only priced risk voluntarily assumed. Index fund managers are excluded rather than payers because their purchase is mandate-compelled, not a case this reading's disclosure-consent logic was built to address; they sit outside the reading's scope rather than inside it as an uncompensated party.
 *
 * MANDATROPHY ANALYSIS:
 *   The disclosure_consent reading resists mandatrophy by keeping legitimacy questions bounded to a specific, dischargeable regulatory duty (adequate disclosure) rather than an open-ended and contestable substantive-fairness standard. If disclosure requirements were later found systematically inadequate to inform actual investment decisions (e.g., empirical findings that risk-factor boilerplate is not read or understood), the founding problem would remain 'live' in name but 'dead' in function — that gap is exactly what founding_problem_status=live plus corroboration from case-law standards (not from founders themselves) is meant to surface for downstream mismatch detection, without this story itself making the substantive-fairness argument that belongs to the sibling reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disclosure_adequacy_vs_actual_comprehension,
    'Does S-1/proxy disclosure of dual-class voting terms produce genuine investor understanding sufficient to ground ''informed consent,'' or does it satisfy only the formal legal disclosure standard while actual comprehension (especially among retail and passively-mandated investors) remains low?',
    'Empirical studies of investor comprehension of risk-factor and governance disclosure sections (e.g., readability analysis, investor survey data, event studies around dual-class IPOs comparing sophisticated vs. retail investor participation and pricing behavior).',
    'If comprehension is empirically low, the disclosure_consent reading''s core premise — that purchase constitutes informed consent — weakens substantially, and the constraint''s effective classification would drift toward the minority_extraction reading''s characterization even though the legal disclosure standard remains formally satisfied.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disclosure_adequacy_vs_actual_comprehension, empirical, 'Whether formal disclosure adequacy tracks actual informed consent.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the disagreement between disclosure_consent and its siblings (founder_stewardship, minority_extraction) live — is it a factual dispute about whether investors are informed, a normative dispute about whether disclosure alone should ground legitimacy (versus substantive parity), or a definitional dispute about what ''legitimacy'' means in securities regulation?',
    'Doctrinal analysis distinguishing procedural legitimacy claims (disclosure-based) from substantive legitimacy claims (parity- or stewardship-based) in securities and corporate law scholarship; comparative analysis of jurisdictions (e.g., Hong Kong, Singapore dual-class listing rules with sunset provisions) that have chosen different resolutions.',
    'If the disagreement is primarily normative/definitional rather than factual, no amount of additional disclosure-comprehension data will resolve the kernel contest — the three readings would remain permanently coexisting rather than one being empirically vindicated over the others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Locating whether the kernel dispute is empirical, normative, or definitional.').

omega_variable(
    index_inclusion_mandate_effect_on_consent,
    'Does the structural compulsion faced by index-tracking and other mandate-constrained investors (who must hold Class A shares of index constituents regardless of governance terms) undermine the disclosure_consent reading''s premise that purchase constitutes voluntary, informed consent for the investor base as a whole?',
    'Data on the proportion of dual-class company float held by mandate-constrained (index, benchmark-tracking) versus fully discretionary investors; analysis of whether index provider decisions (e.g., S&P''s 2017 exclusion of new multi-class listings) already function as an implicit market correction to this problem.',
    'If a large majority of Class A float is mandate-constrained, the disclosure_consent reading''s ''voluntary purchase'' premise applies to a shrinking minority of the actual investor base, weakening the reading''s claim to describe the typical transaction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(index_inclusion_mandate_effect_on_consent, empirical, 'Whether mandate-constrained investors erode the voluntariness premise underlying disclosure-based consent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__disclosure_consent, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__disclosure_consent, theater_ratio, 0, 0.18).
narrative_ontology:measurement(dual_tr_t4, dual_class_legitimacy__disclosure_consent, theater_ratio, 4, 0.2).
narrative_ontology:measurement(dual_tr_t8, dual_class_legitimacy__disclosure_consent, theater_ratio, 8, 0.23).
narrative_ontology:measurement(dual_tr_t12, dual_class_legitimacy__disclosure_consent, theater_ratio, 12, 0.25).
narrative_ontology:measurement(dual_tr_t16, dual_class_legitimacy__disclosure_consent, theater_ratio, 16, 0.27).
narrative_ontology:measurement(dual_tr_t20, dual_class_legitimacy__disclosure_consent, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__disclosure_consent, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(dual_be_t4, dual_class_legitimacy__disclosure_consent, base_extractiveness, 4, 0.3).
narrative_ontology:measurement(dual_be_t8, dual_class_legitimacy__disclosure_consent, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(dual_be_t12, dual_class_legitimacy__disclosure_consent, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(dual_be_t16, dual_class_legitimacy__disclosure_consent, base_extractiveness, 16, 0.37).
narrative_ontology:measurement(dual_be_t20, dual_class_legitimacy__disclosure_consent, base_extractiveness, 20, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(dual_class_legitimacy__disclosure_consent, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__disclosure_consent, resource_allocation).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy__founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy__minority_extraction).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language concept 'dual-class share legitimacy' per the ε-invariance principle: disclosure_consent (this file, ε=0.38, procedural/contractual claim), founder_stewardship (a coordination-function claim about long-horizon mission execution), and minority_extraction (an entitlement claim treating the voting differential itself as extractive, ε expected substantially higher). The three share a kernel (the underlying dual-class arrangement) but instantiate structurally distinct constraints because they answer different questions about what makes the arrangement legitimate or not — process, function, or allocation. Each carries its own ε and stakeholder structure; none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
