% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__creditor_discipline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__creditor_discipline_reading, []).

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
 *   constraint_id: gold_fiat_transition_mechanism__creditor_discipline_reading
 *   human_readable: Creditor-Discipline Reading of the Gold-Fiat Transition: Irredeemable Dollar Claims and Issuer Discretion
 *   domain: monetary economics/political economy/history of economic thought
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the kernel
 *   gold_fiat_transition_mechanism, per the epsilon-invariance principle: the
 *   creditor_discipline_reading holds that the 1971 severance of dollar-gold
 *   convertibility eliminated the creditor redemption veto — the threat of
 *   gold conversion as balance-of-payments discipline — transferring
 *   monetary-sovereignty power from creditor nations to the reserve-currency
 *   issuer. The standing arrangement under contest (the epsilon referent) is
 *   the post-transition order itself as this reading sees it: an issuer that
 *   finances external deficits with irredeemable paper, creditors holding
 *   claims that erode and cannot be cashed, and non-reserve debtors absorbing
 *   adjustment that never lands on the issuer. KEY AGENTS (by structural
 *   relationship): - united_states_monetary_authorities: agenda-setting
 *   issuer (institutional/arbitrage) — administers and collects; -
 *   foreign_creditor_nations: primary target bloc (organized/constrained) —
 *   holds irredeemable claims; - non_reserve_debtor_nations: secondary
 *   targets (moderate/trapped) — absorb imposed adjustment; -
 *   imf_official_sector: administering intermediary
 *   (institutional/constrained) — transmits adjustment to borrowers; -
 *   hard_money_creditor_factions: excluded voice (moderate/trapped) — their
 *   remedy was eliminated; - academic_monetary_historians: analytical
 *   observer — sees the full structure including counterfactuals. The
 *   claim/metrics gap is deliberate: claimed_type is authored from structure
 *   (genuine liquidity coordination plus asymmetric burden plus an active
 *   enforcement layer); the metrics are authored from descriptive operation.
 *   Where computed per-seat classifications diverge from either, that
 *   divergence is the datum.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.76).
domain_priors:suppression_score(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.62).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__creditor_discipline_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__creditor_discipline_reading, "Creditor-Discipline Reading of the Gold-Fiat Transition: Irredeemable Dollar Claims and Issuer Discretion").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__creditor_discipline_reading, "monetary economics/political economy/history of economic thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__creditor_discipline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__creditor_discipline_reading, 'ba474187-09cb-4095-903b-f04fd633ec9f').
narrative_ontology:cs_kernel_codification('ba474187-09cb-4095-903b-f04fd633ec9f', distributed).
narrative_ontology:cs_authority_grounding('ba474187-09cb-4095-903b-f04fd633ec9f', distributed).
narrative_ontology:cs_reading_relation('ba474187-09cb-4095-903b-f04fd633ec9f', gold_fiat_transition_mechanism__automatic_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('ba474187-09cb-4095-903b-f04fd633ec9f', gold_fiat_transition_mechanism__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('ba474187-09cb-4095-903b-f04fd633ec9f', foundational, creditor_redemption_veto_was_primary_balance_of_payments_discipline).
narrative_ontology:cs_axiom_status(creditor_redemption_veto_was_primary_balance_of_payments_discipline, holdable).
narrative_ontology:cs_axiom_grounding('ba474187-09cb-4095-903b-f04fd633ec9f', creditor_redemption_veto_was_primary_balance_of_payments_discipline, empirically_contingent).
narrative_ontology:cs_axiom('ba474187-09cb-4095-903b-f04fd633ec9f', secondary, transition_constituted_geopolitical_power_transfer_to_reserve_issuer).
narrative_ontology:cs_axiom_status(transition_constituted_geopolitical_power_transfer_to_reserve_issuer, holdable).
narrative_ontology:cs_axiom_grounding('ba474187-09cb-4095-903b-f04fd633ec9f', transition_constituted_geopolitical_power_transfer_to_reserve_issuer, empirically_contingent).
narrative_ontology:cs_reference_frame('ba474187-09cb-4095-903b-f04fd633ec9f', bretton_woods_creditor_veto_equilibrium).
narrative_ontology:cs_drift_state('ba474187-09cb-4095-903b-f04fd633ec9f', contemporary_fiat_dollar_order, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('ba474187-09cb-4095-903b-f04fd633ec9f', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, united_states_monetary_authorities).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, imf_official_sector).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, foreign_creditor_nations).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, non_reserve_debtor_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, foreign_creditor_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Federal Reserve and Treasury issue the dollar liabilities the rest of the world holds as reserves and settles trade in. They set interest rates that transmit worldwide, decide which central banks receive swap lines in crises, and operate the sanctions apparatus capable of freezing foreign official holdings. Since 1971 nothing they issue can be redeemed for anything they have not chosen to honor, and no external actor can force adjustment upon them; the gap between what the United States consumes abroad and what it earns is financed by issuing more of its own paper.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, united_states_monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__creditor_discipline_reading, united_states_monetary_authorities, beneficiary).

% Sovereign creditors — export-surplus economies, Gulf energy sellers, aging surplus democracies — accumulated trillion-scale dollar claims on the understanding that convertibility anchored their value; since 1971 those claims are irredeemable and erode with US inflation and depreciation. They continue accumulating because selling large positions would collapse the very assets they hold, and no other market offers comparable depth and liquidity. Many simultaneously run export-led development models that depend on undervalued currencies and on managing dollar demand, and several sit under a US security umbrella that subsidizes their tolerance.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, foreign_creditor_nations, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__creditor_discipline_reading, foreign_creditor_nations, beneficiary).

% Governments and firms outside the reserve-issuing core borrow, invoice, and hold reserves in dollars regardless of their own currency. When US rates rise, refinancing costs spike and capital flees; adjustment arrives through IMF programs with policy conditions attached. They cannot issue debt abroad in their own money, so external shocks always land on them rather than on the issuer — the transition removed the issuer's exposure while leaving theirs intact.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, non_reserve_debtor_nations, payer,
    moderate, biographical, trapped, regional).

% The International Monetary Fund administers balance-of-payments lending denominated in dollars, attaches policy conditions that transmit issuer-compatible adjustment onto borrowing countries, and depends on the dollar-quota system for its own balance sheet. Its centrality and budget grew alongside the maturing dollar system; departing from dollar-denominated operations would render it irrelevant to the crises it exists to manage.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, imf_official_sector, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__creditor_discipline_reading, imf_official_sector, beneficiary).

% Political and intellectual currents — gold-standard advocates, sound-money movements, creditor-class parties — argue for restoring redemption discipline or some hard external anchor. Their preferred lever was eliminated in August 1971 and they hold no seat in modern monetary governance; they publish, campaign, and lobby from outside central bank and treasury decision-making.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, hard_money_creditor_factions, excluded,
    moderate, generational, trapped, national).

% Scholars reconstructing the causes and consequences of the transition from archival records, reserve-drainage data, and counterfactual modeling. They produce the competing readings this corpus separates into distinct files; they collect nothing from the arrangement and answer to no desk inside the system.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, academic_monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__creditor_discipline_reading, united_states_monetary_authorities).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__creditor_discipline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the world a unit of account, settlement medium, and deep safe asset for cross-border trade and finance, plus crisis liquidity through swap lines — solving once, centrally, the problem of settling international transactions and storing value that a national gold stock could no longer solve as trade outgrew metal supply.
% TRANSFER_FUNCTION: Moves real resources and adjustment burden: the issuer obtains foreign goods and assets in exchange for its own irredeemable liabilities, while foreign holders absorb inflation, depreciation, and forced refinancing of their claims; non-reserve debtors additionally absorb externally imposed austerity.
% ABSENT_VOICES: Hard-money creditor factions whose remedy (restored redemption) was the very thing eliminated speak from outside governance; Global South debtor publics bear program conditions without seats in quota governance proportional to exposure; future generations inherit the accumulated unredeemed claims with no present representation.
% DISAPPEARANCE_RATIONALE: If the irredeemable-claim arrangement vanished overnight — say, redemption were restored or the dollar standard replaced — the entire architecture would rearrange: foreign official holders would rush redemption or diversification, Treasury demand would collapse and with it US deficit financing capacity, trade invoicing and settlement would fracture into regional blocks, and dollar-indebted sovereigns would face immediate default cascades.
% FOUNDING_PROBLEM: Bretton Woods faced the Triffin contradiction: world trade growth required expanding dollar liquidity, but confidence in gold convertibility required limiting dollar issuance. Through the 1960s the contradiction matured as gold pools drained; the transition resolved it by severing redemption, converting the constrained liquidity provider into an unconstrained issuer.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting set: BIS Committee on the Global Financial System papers and Annual Reports documenting recurring safe-asset shortage; the Despres-Kindleberger-Triffin debate literature and subsequent academic work on reserve-asset supply elasticity; ECB and PBoC diversification statements acknowledging persistent reliance on the issuer's liabilities. No attesting source sits inside the US policy apparatus.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__creditor_discipline_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__creditor_discipline_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__creditor_discipline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__creditor_discipline_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__creditor_discipline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.76: the issuer's external position yields a persistent return differential (assets abroad out-yield liabilities sold to foreigners — the exorbitant privilege), inflation and depreciation erode creditor claims without recourse, and the 2020s added outright freezing of official reserves. Suppression 0.62: persistence rests on a structural lock-in (network externalities in invoicing and settlement, no comparably deep alternative market) plus an active coercive layer (sanctions exclusion, SWIFT cutoffs, IMF conditionality) — roughly 85% structural, 15% internalized belief in dollar inevitability among policymakers. Theater ratio 0.36: 'strong dollar' rhetoric, ritualized neutrality commitments, and Fed-independence performance coexist with increasingly explicit weaponized deployment; the ratio dips at crisis moments when substance is forced (1981, 2008) and climbs between them. Accessibility collapse 0.42: alternatives exist and are growing (euro, gold re-accumulation, renminbi pilot rails, SDR talk) but none approaches Treasury-market depth, so the option set narrows without vanishing. Resistance 0.55: from the gold-pool raids that preceded the break through Chiang Mai arrangements, post-2022 record official gold purchases, and parallel settlement experiments. The temporal series share one nine-point grid; extractiveness ratchets stepwise at crises (1973, 1981, 2008, 2020) rather than oscillating — each emergency permanently expands issuer tooling (swap lines, facility access), a ratchet, not a cycle, and the oscillation in theater_ratio is a side effect of those same crisis cycles, not an intermittent-reinforcement mechanism.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting issuer seat the arrangement presents as coordination the US builds and backstops — global liquidity, crisis insurance, an open market for everyone's surpluses — so that seat computes a coordination-dominant type. From the foreign-creditor and non-reserve-debtor seats the identical structure operates as irredeemable claims and imported adjustment, computing extraction-dominant. The excluded hard-money seat computes a third thing entirely: an arrangement whose defect is the missing anchor, not the distribution. The engine computes this divergence from power, exit, and role data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   United States monetary authorities: declared beneficiary and agenda_setter with arbitrage-grade exit (no one can force anything on them) — d sits near the beneficiary pole. Foreign creditor nations: declared victims with constrained exit — derivation lands near the target pole, but the override to 0.72 corrects for the double position the derivation cannot see: the same governments losing on their claims simultaneously run export-led models that require managing dollar demand and often sit under US security umbrellas that subsidize tolerance, so they are targets who are also partly subsidized participants, not pure targets. Non-reserve debtor nations: declared victims, trapped exit (cannot issue own-currency debt abroad), no offsetting subsidy — nearest the full-target pole. IMF: declared beneficiary (administrative centrality, quota income) and agenda_setter — low d, though its constrained exit distinguishes it from the issuer's arbitrage. Hard-money factions and academic observers carry no flow attribution.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — supplying elastic global liquidity without a credible external anchor — is still live: every crisis re-concentrates safe-asset demand on the issuer's liabilities, and BIS surveillance keeps rediscovering the shortage. Mandatrophy is therefore NOT declared. The classification work this reading performs is boundary-keeping between mislabels: a pure-snare reading would erase the genuine coordination (the Treasury market really is the world's only deep safe asset, and swap lines really did arrest panics); a pure-rope reading would erase the asymmetric burden (adjustment demonstrably never lands on the issuer). The tangled_rope claim holds both facts in one structure, and the theater_ratio trajectory guards against piton drift should the coordination function ever hollow out while the enforcement shell remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_attribution_delta,
    'Which causal account of the transition governs classification: elimination of creditor veto power (this file, creditor_discipline_reading), replacement of a material constraint with discretionary central bank authority (automatic_constraint_reading), or convergence of multiple independent structural changes with the Nixon Shock as mere symbol (composite_overdetermination_reading)?',
    'Comparative counterfactual reconstruction across the three readings: hold constant the redemption-severance counterfactual versus the technology/labor/legal counterfactuals and test which account predicts the observed reserve-composition, deficit-financing, and adjustment-incidence record.',
    'This story is one reading of kernel gold_fiat_transition_mechanism; the siblings instantiate different constraints with different epsilon attributions. The automatic reading locates the operative structure in central bank discretion itself (lower attributed extraction at the issuer seat, different victim set); the composite reading diffuses causation across telecommunications, labor bargaining, and legal-tender maturation, weakening any single beneficiary/victim pairing. Resolving the contest reassigns the structural delta this file attributes to creditor-veto elimination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_attribution_delta, conceptual, 'Committer structure: this constraint is the creditor_discipline_reading of the gold_fiat_transition_mechanism kernel; siblings automatic_constraint_reading and composite_overdetermination_reading would change the beneficiary/victim structure and epsilon attribution.').

omega_variable(
    creditor_exit_coordination_feasibility,
    'Can foreign creditor nations coordinate diversification quickly enough to escape dollar lock-in before the adjustment costs of moving exceed the holding costs of staying?',
    'Track reserve-composition shifts, official gold purchase records, local-currency settlement corridor volumes, and cross-border payment-rail adoption against estimated threshold models of price impact per seller.',
    'Feasible coordination drops effective suppression for the creditor seat and drifts the arrangement toward symmetric rope; infeasible coordination entrenches lock-in and sharpens the payer-seat extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_exit_coordination_feasibility, empirical, 'Whether the victim bloc possesses actionable coalition power against the lock-in.').

omega_variable(
    privilege_rent_vs_service_compensation,
    'What share of the measured extraction is pure issuer rent versus compensation for services rendered — security provision, open-market access, crisis lender-of-last-resort capacity?',
    'Decompose the exorbitant-privilege return differential into measurable service flows (defense umbrella valuation, market-access premia, crisis facility usage) versus the residual return spread net of risk.',
    'A large service-compensation share supports the tangled_rope coordination framing; a negligible share pushes the arrangement toward snare, since coordination cover would then be thin.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(privilege_rent_vs_service_compensation, conceptual, 'Rent-versus-service decomposition of the issuer''s advantage; determines how much of epsilon is covered by the coordination floor.').

omega_variable(
    sanctions_weaponization_feedback,
    'Does escalating financial enforcement — reserve freezes, SWIFT exclusion, secondary sanctions — strengthen the arrangement by demonstrating that exit is impossible, or undermine it by accelerating investment in alternative rails?',
    'Time-series on non-dollar settlement shares, official-sector gold accumulation, CBDC and alternative-rail pilots before versus after the 2022 reserve-freeze episode.',
    'If enforcement undermines, suppression_requirement peaks and declines as alternatives mature, dating a possible loosening; if it strengthens, the ratchet continues and the payer-seat reading hardens toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctions_weaponization_feedback, empirical, 'Feedback sign between coercive enforcement intensity and the durability of the enforcement layer itself.').

omega_variable(
    triffin_continuation_status,
    'Is the founding liquidity problem still generating the coordination demand that holds this arrangement together, or have alternative safe-asset suppliers materially relieved it?',
    'Safe-asset shortage metrics from BIS CGFS surveillance, auction demand elasticity at Treasury issuance, and substitution elasticities toward euro-area, supranational, and renminbi instruments.',
    'If relieved, the coordination function atrophies and the arrangement drifts toward piton (enforcement shell outliving function); if unresolved, the live founding problem continues to sustain tangled_rope operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_continuation_status, empirical, 'Liveness of the founding Triffin-type problem beneath the current structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__creditor_discipline_reading, 1968, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gft_creditor_tr_t1968, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1968, 0.15).
narrative_ontology:measurement(gft_creditor_tr_t1973, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1973, 0.22).
narrative_ontology:measurement(gft_creditor_tr_t1981, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1981, 0.17).
narrative_ontology:measurement(gft_creditor_tr_t1990, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(gft_creditor_tr_t2001, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2001, 0.31).
narrative_ontology:measurement(gft_creditor_tr_t2008, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2008, 0.23).
narrative_ontology:measurement(gft_creditor_tr_t2015, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(gft_creditor_tr_t2020, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2020, 0.32).
narrative_ontology:measurement(gft_creditor_tr_t2025, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2025, 0.36).

% Extraction over time
narrative_ontology:measurement(gft_creditor_be_t1968, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1968, 0.5).
narrative_ontology:measurement(gft_creditor_be_t1973, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1973, 0.58).
narrative_ontology:measurement(gft_creditor_be_t1981, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1981, 0.61).
narrative_ontology:measurement(gft_creditor_be_t1990, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1990, 0.63).
narrative_ontology:measurement(gft_creditor_be_t2001, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2001, 0.67).
narrative_ontology:measurement(gft_creditor_be_t2008, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2008, 0.7).
narrative_ontology:measurement(gft_creditor_be_t2015, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2015, 0.71).
narrative_ontology:measurement(gft_creditor_be_t2020, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2020, 0.74).
narrative_ontology:measurement(gft_creditor_be_t2025, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2025, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(gft_creditor_su_t1968, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1968, 0.28).
narrative_ontology:measurement(gft_creditor_su_t1973, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1973, 0.34).
narrative_ontology:measurement(gft_creditor_su_t1981, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1981, 0.5).
narrative_ontology:measurement(gft_creditor_su_t1990, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1990, 0.46).
narrative_ontology:measurement(gft_creditor_su_t2001, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2001, 0.44).
narrative_ontology:measurement(gft_creditor_su_t2008, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2008, 0.54).
narrative_ontology:measurement(gft_creditor_su_t2015, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2015, 0.57).
narrative_ontology:measurement(gft_creditor_su_t2020, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2020, 0.59).
narrative_ontology:measurement(gft_creditor_su_t2025, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__creditor_discipline_reading, resource_allocation).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism__automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the end of Bretton Woods': one historical label covering structurally distinct claims, split per the epsilon-invariance principle. automatic_constraint_reading authors the material-to-institutional constraint-type swap; composite_overdetermination_reading authors convergent multi-cause change with the Nixon Shock as symbol; this file authors the power transfer from creditor redemption leverage to reserve-issuer discretion. Each member carries its own epsilon, beneficiary/victim structure, and classification; the upstream members supply causal background that this reading's power-transfer claim presupposes. Cross-file epsilon divergence is expected and is the point of the decomposition, not a defect to reconcile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gold_fiat_transition_mechanism__creditor_discipline_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
