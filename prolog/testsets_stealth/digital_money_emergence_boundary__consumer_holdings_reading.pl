% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__consumer_holdings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: digital_money_emergence_boundary__consumer_holdings_reading
 *   human_readable: Consumer-Holdings Boundary for Digital Money Emergence (EMD Reading)
 *   domain: economic/financial-history/technological
 *
 * SUMMARY:
 *   This story authors ONE reading of the digital-money-emergence kernel: the
 *   claim that digital money came into existence when consumers could
 *   directly hold and transact digital instruments outside traditional bank
 *   accounts, dated to the 1990s e-purse wave and codified by the 2000
 *   Electronic Money Directive. The standing arrangement under contest (the
 *   epsilon referent) is the post-EMD perimeter: a legally bounded category
 *   of consumer-holdable e-money, administered by central bank regulators,
 *   occupied by licensed issuers, and priced into every wallet fee.
 *   Constraint-family note: the conceptualization_reading and
 *   infrastructure_reading siblings are separate constraint files with their
 *   own epsilon values, beneficiary structures, and classifications; this
 *   file hedges nothing across them and links to them only through network
 *   edges.
 *
 * KEY AGENTS:
 *   - central_bank_regulators: agenda-setter and primary beneficiary (institutional/identity_locked) — draws the boundary, supervises licensees, compiles the M4/M5 split
 *   - emi_licensed_issuers: coordinated beneficiary-payer (organized/constrained) — occupies the licensed category, pays compliance, collects category rents
 *   - compliance_industry_intermediaries: derivative beneficiary (organized/mobile) — sells the perimeter's paperwork
 *   - stored_value_card_pioneers: extracted-upon predecessor cohort (organized/trapped) — bore retroactive perimeter costs that closed their schemes
 *   - unlicensed_payment_innovators: burdened-or-excluded entrants (moderate/constrained) — face licensing-or-exit at regressive cost
 *   - retail_fee_payers: diffuse consumer seat (powerless/constrained) — pays embedded compliance costs, receives safeguarding protections
 *   - broad_definition_monetary_economists: excluded definitional rival (moderate/arbitrage) — holds rival periodizations with no governance seat
 *   - monetary_history_analysts: analytical observer (analytical/analytical) — evaluates which boundary explains the record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__consumer_holdings_reading, 0.6).
domain_priors:suppression_score(digital_money_emergence_boundary__consumer_holdings_reading, 0.6).
domain_priors:theater_ratio(digital_money_emergence_boundary__consumer_holdings_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__consumer_holdings_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__consumer_holdings_reading, "Consumer-Holdings Boundary for Digital Money Emergence (EMD Reading)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__consumer_holdings_reading, "economic/financial-history/technological").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__consumer_holdings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__consumer_holdings_reading, '7a198df8-c3bf-49e9-9d39-4d5d07e5711e').
narrative_ontology:cs_kernel_codification('7a198df8-c3bf-49e9-9d39-4d5d07e5711e', distributed).
narrative_ontology:cs_authority_grounding('7a198df8-c3bf-49e9-9d39-4d5d07e5711e', expertise).
narrative_ontology:cs_interpretation_layer_present('7a198df8-c3bf-49e9-9d39-4d5d07e5711e').
narrative_ontology:cs_reading_relation('7a198df8-c3bf-49e9-9d39-4d5d07e5711e', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('7a198df8-c3bf-49e9-9d39-4d5d07e5711e', digital_money_emergence_boundary__infrastructure_reading, coexists_with).
narrative_ontology:cs_axiom('7a198df8-c3bf-49e9-9d39-4d5d07e5711e', foundational, holdability_outside_banks_constitutes_money).
narrative_ontology:cs_axiom_status(holdability_outside_banks_constitutes_money, holdable).
narrative_ontology:cs_axiom_grounding('7a198df8-c3bf-49e9-9d39-4d5d07e5711e', holdability_outside_banks_constitutes_money, conventional).
narrative_ontology:cs_axiom('7a198df8-c3bf-49e9-9d39-4d5d07e5711e', secondary, m4_m5_separation_required).
narrative_ontology:cs_axiom_status(m4_m5_separation_required, holdable).
narrative_ontology:cs_axiom_grounding('7a198df8-c3bf-49e9-9d39-4d5d07e5711e', m4_m5_separation_required, instrumental).
narrative_ontology:cs_reference_frame('7a198df8-c3bf-49e9-9d39-4d5d07e5711e', holdable_instrument_legal_perimeter).
narrative_ontology:cs_drift_state('7a198df8-c3bf-49e9-9d39-4d5d07e5711e', post_stablecoin_perimeter_expansion, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7a198df8-c3bf-49e9-9d39-4d5d07e5711e', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, central_bank_regulators).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, emi_licensed_issuers).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, compliance_industry_intermediaries).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, stored_value_card_pioneers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, unlicensed_payment_innovators).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, retail_fee_payers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, emi_licensed_issuers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Eurosystem and national competent authorities define what counts as e-money, grant and withdraw EMI licenses, supervise safeguarding compliance, and compile the monetary aggregates that separate bank deposits from e-money. The category's existence is their jurisdiction: staffing, budgets, and statistical mandate all attach to the perimeter they drew. Exiting the arrangement would mean abandoning a core statistical and supervisory function the institution has become.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, central_bank_regulators, agenda_setter,
    institutional, generational, identity_locked, continental).

% Fintech firms holding e-money licenses issue prepaid instruments and wallets to consumers, collecting float income and transaction fees inside a legally protected product category. They paid initial capital and safeguarding compliance to enter, and continue to bear recurring compliance costs; in exchange, unlicensed competitors are barred from their market. Leaving the category means surrendering the product line entirely.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, emi_licensed_issuers, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__consumer_holdings_reading, emi_licensed_issuers, payer).

% Law firms, audit practices, and consultancies sell licensing applications, safeguarding audits, and regulatory reporting services to anyone seeking or holding an e-money license. Their revenue scales with the complexity of the perimeter rather than with any outcome it produces, and they can re-skill to serve whatever replacement regime emerges.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, compliance_industry_intermediaries, beneficiary,
    organized, biographical, mobile, continental).

% The 1990s e-purse consortia built consumer-held digital cash products before any perimeter existed, sinking capital into hardware wallets and merchant networks on the assumption that stored value was lawful experimentation. When the 2000 directive codified the category, capital and safeguarding requirements arrived retroactively; most schemes wound down rather than re-license. Their exit was closure, not relocation.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, stored_value_card_pioneers, payer,
    organized, biographical, trapped, global).

% Wallet startups and later stablecoin issuers operate at or beyond the edge of the perimeter, facing a recurring licensing-or-exit choice. Compliance costs scale regressively, falling hardest on small entrants, and some respond by locating in lighter-touch jurisdictions. Their product is consumer-held digital value, which places them inside the boundary's subject matter whether or not they hold a license.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, unlicensed_payment_innovators, payer,
    moderate, biographical, constrained, global).

% Consumers using e-money products pay fees that embed issuer compliance costs, and in return receive safeguarding protections: segregated funds, redemption rights, and insolvency priority. Individual exit means reverting to cash or bank rails, which forfeits the convenience the products provide. They did not participate in drawing the boundary that prices their wallets.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, retail_fee_payers, payer,
    powerless, immediate, constrained, national).

% Scholars who date digital money to infrastructure milestones or to its theoretical conceptualization hold periodizations that carry no weight in official aggregates, licensing decisions, or supervisory practice. They publish across venues and jurisdictions freely, but no seat in statistical governance exists for their definition, and official series are compiled without it.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, broad_definition_monetary_economists, excluded,
    moderate, generational, arbitrage, global).

% Financial historians and monetary economists assessing which emergence boundary does explanatory work across episodes compare the readings against the record of e-purse failures, licensing uptake, and stablecoin growth. They collect no category rents and bear no compliance costs.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, monetary_history_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__consumer_holdings_reading, emi_licensed_issuers).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__consumer_holdings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single official boundary for what counts as digital money: monetary statistics gain a consistent bank-deposit-versus-e-money split (the M4/M5 distinction), consumer holders of prepaid value gain a defined protection regime, and non-bank firms gain a lawful route to issue stored-value instruments.
% TRANSFER_FUNCTION: Moves licensing capital and recurring compliance costs from payment issuers toward supervisory budgets and compliance intermediaries; moves definitional authority and jurisdiction to central banks; moves fee revenue from retail users to licensed issuers; and forecloses market access from unlicensed entrants to licensed incumbents.
% ABSENT_VOICES: Broad-definition monetary economists and open-source cryptocurrency developers were absent when the boundary was codified; both would contest that holdability outside banks is the essence of digital money, and neither held a seat in the directive process or in statistical governance.
% DISAPPEARANCE_RATIONALE: If the boundary vanished overnight, the M4/M5 statistical split would lose its object, EMI licensing across jurisdictions would lose its legal basis, licensed issuers' category rents would evaporate as unlicensed entrants flooded in, the compliance industry's revenue base would collapse, and the fintech market would reorganize around either bank-deposit-inclusive or instrument-neutral definitions.
% FOUNDING_PROBLEM: By the late 1990s, stored-value schemes were issuing consumer-held electronic value outside banking law, creating insolvency exposure for holders, blind spots in monetary statistics, and laundering concerns that no existing category addressed.
% FOUNDING_PROBLEM_CORROBORATION: Central banks attest liveness, citing stablecoin and crypto-asset growth as the perimeter problem recurring at larger scale; independent corroboration from outside the beneficiary set comes from Financial Stability Board and FATF assessments and the academic monetary-economics literature, which document that holder-protection and statistical-perimeter problems persist beyond the licensed category. No party outside the dispute attests the founding problem is fully dead, and none attests it is unchanged.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__consumer_holdings_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__consumer_holdings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__consumer_holdings_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__consumer_holdings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__consumer_holdings_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is moderate-to-substantial (0.60 at interval end) because the boundary layers genuine coordination onto identifiable rents: licensing capital, a compliance service industry whose revenue scales with perimeter complexity, and foreclosure of unlicensed entry that protects incumbent licensees. Suppression (0.60) is authored as a raw structural property and is NOT scaled by power or scope — it reflects the legal-administrative machinery (licensing gates, safeguarding mandates, capital floors) the boundary requires to hold; the engine owns any scaling arithmetic. Theater ratio (0.42) tracks a growing performative share: periodic supervisory reviews and box-ticking safeguarding reports accumulate around a statistical core that remains functional. Accessibility_collapse (0.60) is moderate-high: once the official boundary is adopted, rival periodizations collapse out of policy usage — official statistics, licensing, and supervision all speak consumer-holdings language — while surviving in academia. Resistance (0.50) is steady rather than acute: scholarly dissent, crypto-community rejection of the perimeter, and jurisdiction shopping by issuers. Seat divergence: the regulator seat computes coordination-heavy (it built the standard and collects jurisdiction); the licensee seat computes mixed (paid entry, collects a moat); the pioneer and unlicensed-entrant seats compute extraction-heavy (bore retroactive or regressive costs). Identity-lock binds the regulator seat institutionally: a central bank cannot exit its statistical mandate without dissolving the institutional self-concept that mandate constitutes. Suppression mechanism is almost entirely structural (licensing law, capital rules); the internalized component is negligible. Dynamics are a monotonic ratchet, not a cycle — no intermittent-reinforcement pattern is claimed.
 *
 * PERSPECTIVAL GAP:
 *   From the Eurosystem seat the boundary is a public-good measurement standard that protects consumer holders; from the stored-value pioneer seat it is an ex-post rule change that stranded sunk investment and killed viable schemes; from the excluded-economist seat it is an arbitrary periodization that privileges administrative convenience over economic substance; from the licensed-issuer seat it is a moat purchased at compliance prices. The same statutory text operates as four different constraints depending on seat, and the engine computes that divergence from the structural data rather than from this claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the regulator, licensee, and intermediary seats toward the beneficiary end; victim declarations drive the pioneer, unlicensed-entrant, and consumer seats toward the target end. One override is declared: the powerless atom is set to d=0.55 because the derivation from the retail_fee_payers victim declaration alone would overstate their target position — they pay embedded compliance costs but simultaneously receive the safeguarding protections the perimeter exists to provide, placing them near symmetric. The override applies cleanly because retail_fee_payers is the only powerless-seat stakeholder in this story. Stored-value pioneers sit nearest the full-target end: trapped exit, retroactive cost imposition, scheme closure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — gray-zone stored value exposing holders to insolvency and statistics to blind spots — was live at codification and is now contested: partially solved inside the perimeter while recurring outside it at larger scale in stablecoins. The tangled_rope classification guards against both misreadings: a pure-snare label would erase the real M4/M5 measurement and consumer-protection functions the boundary performs; a pure-rope label would erase the licensing rents, the compliance-industry levy, and the foreclosure of unlicensed entry. R5 mismatch check: founding_problem_status=contested crossed with disappearance_verdict=world_rearranges yields no zombie flag — the arrangement still organizes substantial live activity, so mandatrophy_resolved is not declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is consumer holdability the correct fix for the digital-money-emergence boundary, or do the conceptualization or infrastructure readings fix it better?',
    'Comparative explanatory-power analysis: test which boundary predicts and organizes the record of e-purse adoption, licensing uptake, statistical revisions, and stablecoin growth across episodes the readings date differently.',
    'Adopting the infrastructure reading removes the licensing-perimeter arrangement from this story entirely (the boundary would predate regulation, changing the epsilon referent); adopting the conceptualization reading makes the constraint purely epistemic with no enforcement surface and no beneficiary structure. This file''s classification stands or falls with its reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which sibling reading correctly fixes the emergence kernel; committer-frame routing for the kernel contest.').

omega_variable(
    stablecoin_perimeter_absorption,
    'Does MiCA-style perimeter extension absorb consumer-held stablecoins into the licensed category (the boundary persists with enlarged scope), or does their continued extra-perimeter scale reveal the boundary as superseded (inertial maintenance of an outdated perimeter)?',
    'Observe whether stablecoin issuance migrates into e-money/token licensing at scale or remains extra-perimeter; track the share of consumer-held digital value transacting inside versus outside the licensed category.',
    'Absorption raises the constraint''s effective scope and amplifies extraction through the enlarged verification surface; supersession drives theater_ratio upward and pushes the classification toward piton dynamics — theatrical maintenance of a perimeter the phenomenon has outgrown.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stablecoin_perimeter_absorption, empirical, 'Whether the perimeter absorbs its largest current challenger or is being overtaken by it.').

omega_variable(
    regulator_rent_vs_measurement_public_good,
    'Is the M4/M5 boundary primarily a measurement public good that any modern monetary authority would adopt, or a jurisdiction-expanding construct whose principal effect is to create and allocate regulatory territory?',
    'Counterfactual institutional history: would private or academic statistical conventions have converged on a bank-deposit-versus-e-money split absent the Directive; compare jurisdictions that adopted the category with different supervisory funding models.',
    'Public-good-dominant resolution shifts the weighting toward rope (coordination with modest overhead); rent-dominant resolution confirms the tangled_rope weighting and strengthens the extraction attribution to the agenda-setter seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulator_rent_vs_measurement_public_good, conceptual, 'Whether the boundary''s coordination function or its jurisdiction function dominates.').

omega_variable(
    emd_entry_cost_necessity,
    'Do EMD capital and safeguarding requirements track the genuine cost of protecting consumer holders, or do they exceed that cost enough to function as entry deterrence?',
    'Actuarial comparison of actual safeguarding and wind-up costs against requirement levels, plus failure-rate data for licensed versus unlicensed stored-value schemes across jurisdictions with different thresholds.',
    'Requirements materially above protection cost confirm the extraction component and its regressive incidence on small entrants; parity supports the coordination framing and would soften the victim attribution for unlicensed_payment_innovators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emd_entry_cost_necessity, empirical, 'Whether the perimeter''s price of admission is protection or deterrence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__consumer_holdings_reading, 1985, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1985, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 1985, 0.04).
narrative_ontology:measurement(digi_tr_t1995, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 1995, 0.09).
narrative_ontology:measurement(digi_tr_t2000, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(digi_tr_t2009, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2009, 0.24).
narrative_ontology:measurement(digi_tr_t2015, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(digi_tr_t2020, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2020, 0.36).
narrative_ontology:measurement(digi_tr_t2025, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(digi_be_t1985, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 1985, 0.15).
narrative_ontology:measurement(digi_be_t1995, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 1995, 0.3).
narrative_ontology:measurement(digi_be_t2000, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(digi_be_t2009, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2009, 0.5).
narrative_ontology:measurement(digi_be_t2015, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2015, 0.54).
narrative_ontology:measurement(digi_be_t2020, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2020, 0.57).
narrative_ontology:measurement(digi_be_t2025, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2025, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1985, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 1985, 0.08).
narrative_ontology:measurement(digi_su_t1995, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 1995, 0.15).
narrative_ontology:measurement(digi_su_t2000, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(digi_su_t2009, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2009, 0.48).
narrative_ontology:measurement(digi_su_t2015, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2015, 0.53).
narrative_ontology:measurement(digi_su_t2020, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2020, 0.56).
narrative_ontology:measurement(digi_su_t2025, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2025, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__consumer_holdings_reading, information_standard).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__infrastructure_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial question of when digital money emerged covers three structurally distinct claims that measure different observables. The conceptualization_reading fixes emergence at epistemic availability (a purely intellectual event, negligible extraction, mountain-flavored). The infrastructure_reading fixes it at infrastructural capability (bank-side transfer rails, coordination among institutions, low consumer-facing extraction). This consumer_holdings_reading fixes it at consumer-grade holdability, which is the only reading whose referent includes the post-2000 licensing perimeter — hence the only reading carrying the EMD's enforcement surface, its beneficiary structure (regulators, licensees, compliance intermediaries), and its victim structure (pioneers, unlicensed entrants, fee payers). The upstream readings influence this one chronologically and rhetorically (each is cited in boundary debates), but each file authors its own epsilon over its own referent; no averaging occurs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_emergence_boundary__consumer_holdings_reading, powerless, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
