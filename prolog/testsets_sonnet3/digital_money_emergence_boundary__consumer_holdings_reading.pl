% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__consumer_holdings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__consumer_holdings_reading, []).

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
 *   constraint_id: digital_money_emergence_boundary__consumer_holdings_reading
 *   human_readable: Consumer-Holdings Boundary of Digital Money Emergence (E-Money Directive Reading)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the consumer-holdings reading of the digital
 *   money emergence kernel: money is deemed to have gone digital only at the
 *   point individuals could directly hold and transact with a digital
 *   instrument outside a traditional bank account — marked by 1990s e-purse
 *   products and formalized in the EU's 2000 E-Money Directive (2000/46/EC).
 *   This reading requires a legal and statistical boundary: e-money must be
 *   distinguished from bank deposits (feeding an M4/M5-type aggregate split)
 *   and issuers must obtain a distinct license category (EMI status) rather
 *   than a bank charter. The reading's beneficiaries are the regulatory
 *   bodies who administer this new category and the fintech/e-money issuers
 *   who gain a legitimate, lighter-weight route to market. This is a distinct
 *   constraint from the conceptualization_reading (which locates emergence in
 *   1960s-1980s theoretical/cryptographic work, with academic and
 *   cryptographic-community beneficiaries) and the infrastructure_reading
 *   (which locates emergence in 1960s-1970s electronic transfer rails like
 *   ATMs, ACH, and SWIFT, with beneficiaries among interbank infrastructure
 *   operators). Per the ε-invariance principle, these are not the same
 *   constraint measured three ways: the consumer-holdings boundary has its
 *   own beneficiary set, its own victim set (individual holders bearing a
 *   weaker protection regime), and its own extraction profile, and is
 *   authored here as its own file, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - regulatory_bodies_emi_ecb: Primary agenda-setter (institutional/analytical) — defines and administers the legal boundary
 *   - fintech_issuers: Primary beneficiary (organized/mobile) — gains a distinct, lighter-weight licensing category
 *   - e_purse_consumers: Primary target (powerless/constrained) — holds balances with weaker protection than bank deposits
 *   - unbanked_e_money_holders: Most exposed target (powerless/trapped) — depends entirely on e-money with no banking fallback
 *   - central_bank_statisticians: Analytical observer — must classify e-money within monetary aggregates
 *   - traditional_banks: Excluded competitor — objected to an uneven playing field in consultation but the boundary was drawn around fintech entry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__consumer_holdings_reading, 0.52).
domain_priors:suppression_score(digital_money_emergence_boundary__consumer_holdings_reading, 0.44).
domain_priors:theater_ratio(digital_money_emergence_boundary__consumer_holdings_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__consumer_holdings_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__consumer_holdings_reading, "Consumer-Holdings Boundary of Digital Money Emergence (E-Money Directive Reading)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__consumer_holdings_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__consumer_holdings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__consumer_holdings_reading, 'a8562965-d498-4095-9508-8beda91705c7').
narrative_ontology:cs_kernel_codification('a8562965-d498-4095-9508-8beda91705c7', formalized).
narrative_ontology:cs_authority_grounding('a8562965-d498-4095-9508-8beda91705c7', extraction).
narrative_ontology:cs_interpretation_layer_present('a8562965-d498-4095-9508-8beda91705c7').
narrative_ontology:cs_reading_relation('a8562965-d498-4095-9508-8beda91705c7', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('a8562965-d498-4095-9508-8beda91705c7', digital_money_emergence_boundary__infrastructure_reading, influences).
narrative_ontology:cs_axiom('a8562965-d498-4095-9508-8beda91705c7', foundational, moneyness_requires_individual_direct_holding).
narrative_ontology:cs_axiom_status(moneyness_requires_individual_direct_holding, holdable).
narrative_ontology:cs_axiom_grounding('a8562965-d498-4095-9508-8beda91705c7', moneyness_requires_individual_direct_holding, conventional).
narrative_ontology:cs_axiom('a8562965-d498-4095-9508-8beda91705c7', secondary, legal_category_creation_is_necessary_for_emergence).
narrative_ontology:cs_axiom_status(legal_category_creation_is_necessary_for_emergence, holdable).
narrative_ontology:cs_axiom_grounding('a8562965-d498-4095-9508-8beda91705c7', legal_category_creation_is_necessary_for_emergence, conventional).
narrative_ontology:cs_reference_frame('a8562965-d498-4095-9508-8beda91705c7', pre_emd_undefined_stored_value_status).
narrative_ontology:cs_drift_state('a8562965-d498-4095-9508-8beda91705c7', post_2009_emd_recast_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a8562965-d498-4095-9508-8beda91705c7', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, e_money_institutions).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, regulatory_bodies_emi_ecb).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, fintech_issuers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, e_purse_consumers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, unbanked_e_money_holders).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, small_prepaid_issuers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, small_prepaid_issuers).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__consumer_holdings_reading, consumer_holding_criterion_of_moneyness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines the legal category of electronic money (the EU E-Money Directive, 2000/46/EC and successors) and draws the line that determines whether a digital instrument counts as 'money' requiring prudential regulation, safeguarding of funds, and separate statistical treatment (M4/M5-type distinctions from bank deposits). Sets licensing categories (EMI status) that fintech issuers must obtain to operate legally. Does not hold consumer risk directly but administers the boundary and can redraw it.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, regulatory_bodies_emi_ecb, agenda_setter,
    institutional, generational, analytical, continental).

% E-money institutions and prepaid/e-purse issuers that benefit from a legally recognized category distinct from banking: lighter capital requirements than a bank charter, a defined product they can market as 'digital money' with regulatory legitimacy, and a moat against being classified either as unlicensed deposit-taking (illegal) or as fully regulated banks (costly). Can relocate licensing jurisdiction within the EU passporting regime if one regulator is unfavorable.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, fintech_issuers, beneficiary,
    organized, biographical, mobile, continental).

% Individuals holding balances on e-purses, prepaid cards, or e-money accounts. Because e-money is legally distinguished from a bank deposit, their holdings typically lack deposit-guarantee-scheme protection even though the funds look and function like money in daily use. If the issuer fails, safeguarding rules are a weaker protection than deposit insurance. Cannot easily verify which protections apply without specialist knowledge of the boundary the regulation draws.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, e_purse_consumers, payer,
    powerless, immediate, constrained, national).

% People who use e-money products as a substitute for a bank account because they cannot access or afford traditional banking. They depend entirely on the e-money category's legitimacy for their day-to-day transacting, but bear the full consequence of the category's lighter protective regime — they have no traditional bank fallback and cannot 'exit' into banking without the resources banking requires.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, unbanked_e_money_holders, payer,
    powerless, biographical, trapped, national).

% Smaller e-money issuers who benefit from the category's existence (it lets them operate at all) but bear disproportionate compliance costs relative to large issuers when the boundary's rules tighten — safeguarding audits, capital thresholds, and reporting obligations scaled for institutional issuers strain smaller balance sheets. Exiting the category means shutting down the product line.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, small_prepaid_issuers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__consumer_holdings_reading, small_prepaid_issuers, beneficiary).

% Maintain the monetary aggregates (M1 through M4/M5) and must decide whether e-money balances count as narrow money, broad money, or a separate aggregate entirely. Their classification choices feed monetary policy transmission analysis without their having a stake in the underlying commercial dispute.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, central_bank_statisticians, observer,
    institutional, generational, analytical, continental).

% Compete with e-money issuers for transaction volume and float but were not the primary constituency the E-Money Directive was drafted to serve; their objection — that e-money issuers operate deposit-like products under a lighter regulatory regime, creating an uneven playing field — was heard in consultation but the boundary was drawn to enable fintech entry rather than to equalize the two regimes.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, traditional_banks, excluded,
    institutional, generational, arbitrage, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__consumer_holdings_reading, fintech_issuers).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__consumer_holdings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a workable legal and statistical boundary so that digital balances individuals hold and spend directly can be issued, regulated, and counted as a distinct instrument from bank deposits — solving a real classification problem for supervisors, issuers, and consumers who need to know what protections (or lack thereof) attach to a given balance.
% TRANSFER_FUNCTION: Moves regulatory legitimacy and market access to fintech e-money issuers and defines the terms under which consumer digital balances receive (lesser) protection than bank deposits; the coordination benefit of a defined product category is captured largely by issuers and regulators, while the protection gap relative to deposit insurance is borne by the consumers who hold the balances.
% ABSENT_VOICES: Ordinary e-purse and e-money holders, especially unbanked users who rely on these products as their only transacting instrument, were not meaningfully represented in the directive's drafting process, which was driven by industry consultation and central-bank/supervisory technical concerns about prudential categorization.
% DISAPPEARANCE_RATIONALE: If the consumer-holdings legal boundary vanished, e-money issuers would lose their distinct licensing category and would have to be reclassified either as unlicensed deposit-takers (forcing closure or full bank licensing) or as unregulated non-financial float-holders (removing consumer protections entirely); central bank statisticians would lose the M4/M5-type distinction and monetary aggregates would need re-derivation; millions of prepaid and e-wallet balances across the EU would face immediate legal uncertainty.
% FOUNDING_PROBLEM: In the 1990s, e-purse and prepaid stored-value products proliferated without any clear legal status: were they bank deposits requiring banking licenses, unregulated IOUs, or something else? Supervisors needed a category to prevent both regulatory arbitrage (unlicensed deposit-taking) and stifling innovation by forcing every issuer into full bank charters.
% FOUNDING_PROBLEM_CORROBORATION: The European Commission and EBA continue to assert the category remains necessary to enable fintech innovation while protecting consumers from unlicensed deposit-taking. Independent consumer-protection researchers and deposit-insurance scholars outside the fintech and regulatory establishment have documented that e-money safeguarding provisions offer materially weaker protection than deposit guarantee schemes, and argue the original problem (undefined legal status) has been solved but has been replaced by a durable protection gap that serves issuer cost savings as much as consumer need.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__consumer_holdings_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__consumer_holdings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__consumer_holdings_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__consumer_holdings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__consumer_holdings_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__consumer_holdings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_emergence_boundary__consumer_holdings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_emergence_boundary__consumer_holdings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52) sits at a moderate level: the category creation genuinely solves a real classification problem (avoiding forcing every prepaid issuer into a full bank charter, avoiding leaving consumers with zero legal protection), but the same boundary systematically produces a protection gap — e-money safeguarding rules are lighter than deposit guarantee schemes — that saves issuers compliance cost at consumer expense. Suppression (0.44) reflects that consumers cannot easily exit the category once they hold e-money balances (redraw of the boundary is a regulatory, not individual, choice) but is not extreme because e-money remains largely voluntary relative to necessity goods. Theater (0.28) is present but modest: safeguarding requirements are substantively enforced, not purely cosmetic, though disclosure of the deposit-insurance gap to ordinary consumers is often thin. Accessibility collapse (0.5) and resistance (0.4) reflect a boundary that is now largely settled among regulators and industry, though consumer-protection researchers and deposit-insurance scholars continue to contest whether the gap is justified.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory bodies and fintech issuers derive as beneficiaries: the former administer and gain jurisdictional relevance from a new regulatory category, the latter gain market access at lower capital cost than a bank charter — both sit near the beneficiary end of directionality. E-purse consumers and unbanked e-money holders are victims: they hold the actual balances subject to the weaker protection regime, with the unbanked holders overridden toward the full-target end because they are structurally trapped (no banking fallback) rather than merely constrained. Small prepaid issuers are dual-positioned: they benefit from the category's existence but bear disproportionate compliance costs at their scale relative to large issuers, which the moderate power/constrained exit combination captures without needing an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (undefined legal status for 1990s stored-value products) is genuinely dead as originally stated — regulators, courts, and issuers now agree e-money is a defined legal category. But the arrangement persists past that resolution because it now also performs an ongoing function: enabling issuers to offer deposit-like products without deposit-like protections. The founding_problem_status is authored as 'contested' rather than 'dead' because regulatory bodies maintain the category is still solving a live innovation-enablement problem, while consumer-protection researchers hold the original problem is solved and the persisting protection gap is a separate, unaddressed harm riding on the original justification. This divergence between stated purpose and current effect is exactly what the tangled_rope classification and this omega-routed commentary are meant to surface without adjudicating.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consumer_holding_boundary_indeterminacy,
    'Is the consumer-holdings criterion (individual direct holding outside a bank account) the correct structural marker for digital money''s emergence, or is it a regulatory line drawn for administrative convenience rather than economic substance?',
    'Comparative analysis of whether e-money balances function economically identically to bank deposits from the holder''s perspective (fungibility, transaction finality, acceptance) versus whether the legal distinction tracks a genuine difference in the underlying claim''s nature.',
    'If the boundary tracks genuine economic substance, the tangled_rope classification''s coordination function is stronger (real problem, real solution). If the boundary is primarily administrative convenience that happens to produce lighter regulatory treatment, the extraction component dominates and the constraint drifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_holding_boundary_indeterminacy, conceptual, 'Whether the consumer-holdings criterion is economically substantive or an administratively convenient line.').

omega_variable(
    sibling_reading_displacement,
    'Does the consumer-holdings reading''s later dating (2000, EMD) versus the infrastructure reading''s earlier dating (1967-1977) or the conceptualization reading''s earliest dating (1960s-1985) reflect three genuinely independent emergence events, or does it reflect retrospective boundary-drawing by whichever community wants to claim priority or avoid regulatory attention?',
    'Track which reading each institutional actor (central banks, fintech trade associations, cryptography historians) invokes in policy and historical documents, and whether the choice correlates with that actor''s interest in an earlier or later emergence date.',
    'If reading choice correlates with actor interest, all three readings should be treated as contested framings rather than settled historical fact, reinforcing the need to keep them as separate, non-averaged constraint files per the ε-invariance principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_displacement, conceptual, 'Whether the three kernel readings reflect independent facts or interest-driven boundary selection.').

omega_variable(
    protection_gap_justification,
    'Is the lighter safeguarding regime for e-money (relative to deposit guarantee schemes) a justified proportionate response to lower systemic risk, or an unjustified extraction that externalizes issuer failure risk onto consumers?',
    'Empirical study of e-money issuer failure rates and consumer losses relative to bank failure rates and insured losses, controlling for balance size and consumer demographics (particularly unbanked holders).',
    'If losses are proportionate to the lighter risk profile, the tangled_rope''s extraction component is smaller than currently authored. If unbanked holders suffer disproportionate uninsured losses, the victim classification and extraction magnitude are understated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(protection_gap_justification, empirical, 'Whether the consumer protection gap is proportionate to actual risk or an unjustified cost externalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__consumer_holdings_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1990, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement_basis(digi_tr_t1990, observed).
narrative_ontology:measurement(digi_tr_t2000, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement_basis(digi_tr_t2000, observed).
narrative_ontology:measurement(digi_tr_t2009, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2009, 0.2).
narrative_ontology:measurement_basis(digi_tr_t2009, observed).
narrative_ontology:measurement(digi_tr_t2015, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2015, 0.24).
narrative_ontology:measurement_basis(digi_tr_t2015, observed).
narrative_ontology:measurement(digi_tr_t2020, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2020, 0.27).
narrative_ontology:measurement_basis(digi_tr_t2020, observed).
narrative_ontology:measurement(digi_tr_t2024, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(digi_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(digi_be_t1990, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 1990, 0.2).
narrative_ontology:measurement_basis(digi_be_t1990, observed).
narrative_ontology:measurement(digi_be_t2000, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2000, 0.32).
narrative_ontology:measurement_basis(digi_be_t2000, observed).
narrative_ontology:measurement(digi_be_t2009, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2009, 0.4).
narrative_ontology:measurement_basis(digi_be_t2009, observed).
narrative_ontology:measurement(digi_be_t2015, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2015, 0.46).
narrative_ontology:measurement_basis(digi_be_t2015, observed).
narrative_ontology:measurement(digi_be_t2020, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2020, 0.5).
narrative_ontology:measurement_basis(digi_be_t2020, observed).
narrative_ontology:measurement(digi_be_t2024, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2024, 0.52).
narrative_ontology:measurement_basis(digi_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1990, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement_basis(digi_su_t1990, observed).
narrative_ontology:measurement(digi_su_t2000, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2000, 0.25).
narrative_ontology:measurement_basis(digi_su_t2000, observed).
narrative_ontology:measurement(digi_su_t2009, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2009, 0.35).
narrative_ontology:measurement_basis(digi_su_t2009, observed).
narrative_ontology:measurement(digi_su_t2015, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement_basis(digi_su_t2015, observed).
narrative_ontology:measurement(digi_su_t2020, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2020, 0.43).
narrative_ontology:measurement_basis(digi_su_t2020, observed).
narrative_ontology:measurement(digi_su_t2024, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2024, 0.44).
narrative_ontology:measurement_basis(digi_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__consumer_holdings_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__consumer_holdings_reading, 0.1).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__infrastructure_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints in the digital_money_emergence_boundary kernel family. conceptualization_reading dates emergence to 1960s-1985 theoretical work (beneficiary: cryptographic/academic community); infrastructure_reading dates emergence to 1967-1977 electronic transfer infrastructure (beneficiary: interbank infrastructure operators); consumer_holdings_reading (this file) dates emergence to 1990s-2000 consumer-held instruments (beneficiary: regulators and fintech issuers). Each reading has a distinct ε: the infrastructure reading's ε is likely lower (interbank rails are widely regarded as pure coordination with minimal contested beneficiary capture), the conceptualization reading's ε is likely near-zero (academic priority claims extract little), while this reading's ε (0.52) reflects the consumer protection gap embedded in the E-Money Directive's licensing architecture. The three are linked, not merged, per Rule 1 of the committer frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
