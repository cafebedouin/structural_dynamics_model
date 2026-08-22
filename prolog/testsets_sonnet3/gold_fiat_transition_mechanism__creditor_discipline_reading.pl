% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__creditor_discipline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Nixon Shock as Elimination of Creditor Redemption Discipline (Creditor-Discipline Reading)
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   This story instantiates the creditor-discipline reading of the gold-fiat
 *   transition kernel: the August 1971 suspension of dollar-gold
 *   convertibility is read as a deliberate elimination of creditor-nation
 *   veto power over US fiscal and monetary policy. Under the Bretton Woods
 *   gold-exchange standard, surplus nations (notably France) could threaten
 *   sustained gold redemption to force balance-of-payments discipline on the
 *   reserve issuer. This reading holds that the transition's decisive
 *   structural feature is the unilateral removal of that leverage,
 *   redistributing effective monetary sovereignty from creditor central banks
 *   to the reserve-currency issuer and, secondarily, to debtor nations
 *   generally. This is deliberately NOT the automatic_constraint_reading
 *   (which frames the change as material-to-institutional constraint
 *   substitution, agnostic on winners) nor the
 *   composite_overdetermination_reading (which denies the Nixon Shock is a
 *   causal node at all, treating it as a symbolic marker of independently
 *   converging trends). All three are separate constraint stories sharing one
 *   kernel; ε differs sharply across them because they are answering
 *   different structural questions about the same historical episode, not
 *   measuring the same claim from different angles.
 *
 * KEY AGENTS:
 *   - us_treasury_and_federal_reserve: primary beneficiary and agenda-setter (institutional/arbitrage) — gains discretionary latitude by removing the redemption obligation
 *   - creditor_nations_holding_dollar_reserves: primary victim (powerful/trapped) — loses disciplinary leverage and holds devalued claims
 *   - gold_bloc_central_banks: secondary victim (institutional/constrained) — absorbs adjustment costs of an unplanned regime change
 *   - debtor_nations_generally: secondary beneficiary (moderate/constrained) — gains derivative fiscal room
 *   - later_monetary_historians: analytical observer — adjudicates between competing readings of the same episode
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.78).
domain_priors:suppression_score(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.62).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__creditor_discipline_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__creditor_discipline_reading, "Nixon Shock as Elimination of Creditor Redemption Discipline (Creditor-Discipline Reading)").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__creditor_discipline_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__creditor_discipline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__creditor_discipline_reading, 'ae3f34ac-3d78-4e67-a853-b62730974181').
narrative_ontology:cs_kernel_codification('ae3f34ac-3d78-4e67-a853-b62730974181', distributed).
narrative_ontology:cs_authority_grounding('ae3f34ac-3d78-4e67-a853-b62730974181', distributed).
narrative_ontology:cs_reading_relation('ae3f34ac-3d78-4e67-a853-b62730974181', gold_fiat_transition_mechanism__automatic_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae3f34ac-3d78-4e67-a853-b62730974181', gold_fiat_transition_mechanism__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('ae3f34ac-3d78-4e67-a853-b62730974181', foundational, redemption_threat_constituted_real_creditor_leverage).
narrative_ontology:cs_axiom_status(redemption_threat_constituted_real_creditor_leverage, holdable).
narrative_ontology:cs_axiom_grounding('ae3f34ac-3d78-4e67-a853-b62730974181', redemption_threat_constituted_real_creditor_leverage, empirically_contingent).
narrative_ontology:cs_axiom('ae3f34ac-3d78-4e67-a853-b62730974181', foundational, unilateral_suspension_was_distributive_not_merely_technical).
narrative_ontology:cs_axiom_status(unilateral_suspension_was_distributive_not_merely_technical, holdable).
narrative_ontology:cs_axiom_grounding('ae3f34ac-3d78-4e67-a853-b62730974181', unilateral_suspension_was_distributive_not_merely_technical, empirically_contingent).
narrative_ontology:cs_reference_frame('ae3f34ac-3d78-4e67-a853-b62730974181', bretton_woods_gold_exchange_standard).
narrative_ontology:cs_drift_state('ae3f34ac-3d78-4e67-a853-b62730974181', camp_david_suspension_1971, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('ae3f34ac-3d78-4e67-a853-b62730974181', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, us_treasury_and_federal_reserve).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations_generally).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations_holding_dollar_reserves).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_bloc_central_banks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, domestic_us_wage_earners).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, domestic_us_wage_earners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Announces suspension of gold convertibility unilaterally, ending the obligation to redeem foreign-held dollars for gold at $35/oz. Retains the ability to issue reserve-currency liabilities without a physical redemption constraint, financing deficits and expanding the money supply without the balance-of-payments discipline that gold convertibility had imposed. As reserve-currency issuer, gains a structural advantage no other holder of the new arrangement gains.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, us_treasury_and_federal_reserve, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__creditor_discipline_reading, us_treasury_and_federal_reserve, beneficiary).

% Sovereign borrowers whose fiscal room had been indirectly bounded by a system in which reserve assets ultimately traced to convertible dollars. With convertibility gone, the anchor loosens; debtor states gain relative room to run deficits and devalue without triggering the same automatic reserve drain a gold-linked system would have forced. Benefit is real but secondary and derivative of the primary beneficiary's position.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations_generally, beneficiary,
    moderate, generational, constrained, global).

% France, and other surplus nations that had accumulated dollar reserves under Bretton Woods expecting redemption rights in gold at a fixed rate. The August 1971 announcement extinguishes that redemption option retroactively for reserves already held; they are left holding a depreciating paper claim with no exit into the promised asset. Their prior leverage — the credible threat of a gold run that disciplined US deficit spending — is unilaterally revoked without negotiation or compensation.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations_holding_dollar_reserves, payer,
    powerful, biographical, trapped, global).

% Central banks (West Germany, Switzerland, others) that had organized policy around dollar convertibility as an anchor for their own currencies. Forced to improvise floating-rate regimes or new pegs on short notice, absorbing the adjustment costs of a system change they did not choose and had actively tried to forestall through prior gold-window pressure.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_bloc_central_banks, payer,
    institutional, biographical, constrained, continental).

% Experience the downstream 1970s inflation partly attributable to the removed discipline, while also benefiting indirectly from any employment effects of expanded fiscal and monetary latitude. Have no seat in the decision and no exit from the currency they are paid and taxed in.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, domestic_us_wage_earners, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__creditor_discipline_reading, domestic_us_wage_earners, beneficiary).

% Private banks and multinational treasuries operating the growing offshore dollar market that had already been arbitraging around gold-window pressure. Not consulted in the Camp David decision but structurally positioned to profit from the resulting volatility and float-driven trading opportunities; their prior arbitrage activity is one of the pressures the creditor-discipline reading treats as secondary to the deliberate policy choice.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, eurodollar_market_participants, excluded,
    organized, biographical, arbitrage, global).

% Assess the episode retrospectively, weighing whether the abandonment of convertibility is best read as a deliberate power transfer from creditor to reserve-issuer, an automatic-constraint story, or an overdetermined convergence of independent causes.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, later_monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__creditor_discipline_reading, us_treasury_and_federal_reserve).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__creditor_discipline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None genuine at the moment of the decision from this reading's perspective: the gold-exchange standard had coordinated international settlement by giving every reserve holder a common, physically-scarce unit of ultimate account. This reading holds that the coordination value existed but was subordinated to, and ultimately sacrificed for, the redistribution of veto power.
% TRANSFER_FUNCTION: Moves effective fiscal and monetary discretion from creditor nations (who could previously threaten a gold run to discipline US deficits) to the United States as reserve-currency issuer, and secondarily to debtor nations generally who benefit from a looser global anchor; the corresponding cost is borne by creditor-nation reserve holders whose accumulated claims are devalued and whose disciplinary leverage disappears without compensation.
% ABSENT_VOICES: Creditor-nation legislatures and central bank governing boards were not consulted before the Camp David announcement; France's prior gold-redemption requests were the proximate trigger the decision was designed to preempt rather than negotiate with. Eurodollar market participants who had been pricing the strain for years were also outside the room.
% DISAPPEARANCE_RATIONALE: Had the creditor veto not been eliminated, sustained gold demands from surplus nations would have forced earlier and more binding fiscal discipline on the reserve issuer, likely constraining deficit spending, altering the trajectory of the Eurodollar market's growth, and changing the relative bargaining power of creditor states in subsequent monetary negotiations (e.g., special drawing rights design, EMS formation).
% FOUNDING_PROBLEM: The immediate problem the suspension solved, on this reading, was the erosion of US gold reserves under mounting redemption pressure from creditor nations exercising exactly the disciplinary leverage the Bretton Woods system had given them — a leverage the reserve issuer found increasingly intolerable as its own deficits and troop-financing commitments grew.
% FOUNDING_PROBLEM_CORROBORATION: French officials and later European central bankers (outside the US Treasury) attested contemporaneously that the gold run was a deliberate exercise of disciplinary leverage against US deficit spending, not merely a technical liquidity event; this corroboration comes from the party whose leverage was extinguished, which the reading treats as more credible on this point than the US's own contemporaneous framing of the decision as a technical necessity.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__creditor_discipline_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__creditor_discipline_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__creditor_discipline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.78, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.78 by 1975) because, on this reading, the suspension is a one-sided abrogation of an implicit bargain: creditor nations had accepted dollar accumulation partly on the strength of the redemption guarantee, and that guarantee was withdrawn without negotiated compensation, while the withdrawing party retained and expanded its own discretion. Suppression is authored moderate-high (0.62 by end of interval) reflecting the diplomatic and structural pressure (e.g., threat of trade retaliation, dollar's continued reserve-currency centrality) that discouraged creditor nations from unwinding dollar holdings even after the guarantee's removal — this is a raw structural property of the arrangement's persistence, not scaled by the reserve issuer's power in the metric itself (only in the engine's later χ computation). Theater ratio is kept low-moderate (0.28) because the underlying transfer of leverage is substantive, not primarily performative, though some of the '15% cosmetic gold surcharge' negotiations (Smithsonian Agreement) that followed did have theatrical elements without altering the underlying discretion transfer.
 *
 * PERSPECTIVAL GAP:
 *   From the US Treasury seat, the 1971 decision reads as necessary sovereign self-defense against an untenable and asymmetric redemption obligation — a rope cutting free of an outdated commitment. From the creditor-nation seat, the identical event reads as unilateral extraction: a promise honored only until honoring it became inconvenient for the promising party, after which it was withdrawn by fiat with no recourse for the counterparties who had relied on it. The engine's per-seat computation should register the US seat closer to rope/mountain-like framing (removing a constraint it experienced as burdensome) while creditor seats compute closer to snare/tangled_rope (a structure that continued to require diplomatic and market-structural coercion to sustain, e.g., continued dollar centrality in trade invoicing, after the original guarantee was gone).
 *
 * DIRECTIONALITY LOGIC:
 *   The US Treasury/Fed sits at the strong-beneficiary end: it wrote the rule, benefits directly from the removed constraint, and retains arbitrage-grade exit (it can always choose future policy unilaterally). Creditor nations sit at the strong-target end: trapped, because unwinding dollar reserves at scale would itself destabilize the system they depend on for trade settlement — their exit was foreclosed by the same event that extracted from them. Gold bloc central banks are constrained rather than trapped because they retained some latitude to construct alternative pegs (the 'Snake', later EMS), but bore real transition costs they did not choose. Debtor nations generally are a diffuse, secondary beneficiary class — real benefit, but derivative of and smaller than the reserve issuer's gain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an unsustainable run on US gold reserves — was genuinely live in 1971 (the automatic_constraint_reading and this reading agree on that much) but this reading holds the SOLUTION chosen (unilateral suspension without renegotiation of creditor claims) converted what could have been a jointly-managed transition into a unilateral power grab. The founding problem is coded 'dead' here in the specific sense that the redemption-pressure crisis was resolved by 1973, yet the asymmetric discretion the resolution created for the reserve issuer persisted and hardened for decades afterward — exactly the mandatrophy signature: a crisis-response structure outliving the crisis and becoming a standing extraction of monetary sovereignty rather than reverting once the acute pressure passed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deliberate_choice_vs_forced_necessity,
    'Was the August 1971 suspension a deliberate exercise of the reserve issuer''s latent power to redistribute leverage, or the least-bad response to an already-unsustainable gold drain that any reserve issuer would have been forced into?',
    'Archival examination of Treasury and Fed internal deliberations (Volcker Group memos, Camp David meeting records) for evidence of anticipated distributive consequences versus purely defensive framing; comparison with counterfactual policy options available in 1971 (e.g., negotiated gold price revaluation, IMF-mediated multilateral restructuring) that were considered and rejected.',
    'If deliberate and distributively motivated, this reading''s high-extraction, tangled_rope-leaning classification is well-supported. If purely defensive with no distributive intent, this reading''s ε may be overstated relative to the automatic_constraint_reading, which would then be the more structurally accurate framing of the same historical event.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberate_choice_vs_forced_necessity, empirical, 'Whether the veto-elimination was strategically intended or a forced defensive necessity.').

omega_variable(
    creditor_leverage_realism,
    'How real was the creditor nations'' pre-1971 veto power in practice — could France and others have actually forced a change in US policy through sustained gold redemption, or was the ''threat'' already hollow given the scale of Eurodollar holdings outside any single nation''s control?',
    'Quantitative reconstruction of gold-redemption capacity versus total dollar liabilities outstanding in 1971, and historical assessment of whether coordinated creditor action (a genuine multilateral redemption run) was organizationally feasible given competing national interests among gold bloc states.',
    'If the leverage was already largely symbolic before 1971 (undermined by Eurodollar market growth — the composite_overdetermination_reading''s preferred causal factor), then this reading''s beneficiary/victim structure describes the formalization of an already-accomplished shift rather than the shift itself, which would lower confidence in the high ε authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(creditor_leverage_realism, conceptual, 'Whether the creditor veto power this reading centers was still substantively operative at the moment of its formal elimination.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the correct unit of analysis a single decisive policy act (favoring this reading and the automatic_constraint_reading) or a converging bundle of independent structural trends (favoring the composite_overdetermination_reading) — and does the choice of unit change which agents count as beneficiaries/victims at all?',
    'Cross-reading comparison: hold constant the historical record and test whether removing the Nixon announcement counterfactually (leaving telecommunications, labor bargaining, and Bretton Woods peg pressures in place) still produces a comparable creditor-to-issuer leverage shift on a longer timescale.',
    'If the composite reading is correct, the sharp beneficiary/victim structure authored here (US Treasury vs. creditor nations) dissolves into a more diffuse set of winners and losers across telecom-enabled capital arbitrageurs, labor, and central banks generally — a materially different constraint with a much lower single-actor-attributable ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel is better modeled as one decisive act or a composite of independent causes; this reading commits to the former.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__creditor_discipline_reading, 1958, 1985).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1958, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1958, 0.1).
narrative_ontology:measurement_basis(gold_tr_t1958, observed).
narrative_ontology:measurement(gold_tr_t1965, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement_basis(gold_tr_t1965, observed).
narrative_ontology:measurement(gold_tr_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1971, 0.2).
narrative_ontology:measurement_basis(gold_tr_t1971, observed).
narrative_ontology:measurement(gold_tr_t1975, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1975, 0.25).
narrative_ontology:measurement_basis(gold_tr_t1975, observed).
narrative_ontology:measurement(gold_tr_t1980, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1980, 0.27).
narrative_ontology:measurement_basis(gold_tr_t1980, observed).
narrative_ontology:measurement(gold_tr_t1985, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1985, 0.28).
narrative_ontology:measurement_basis(gold_tr_t1985, observed).

% Extraction over time
narrative_ontology:measurement(gold_be_t1958, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1958, 0.35).
narrative_ontology:measurement_basis(gold_be_t1958, observed).
narrative_ontology:measurement(gold_be_t1965, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1965, 0.48).
narrative_ontology:measurement_basis(gold_be_t1965, observed).
narrative_ontology:measurement(gold_be_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1971, 0.7).
narrative_ontology:measurement_basis(gold_be_t1971, observed).
narrative_ontology:measurement(gold_be_t1975, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1975, 0.78).
narrative_ontology:measurement_basis(gold_be_t1975, observed).
narrative_ontology:measurement(gold_be_t1980, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1980, 0.76).
narrative_ontology:measurement_basis(gold_be_t1980, observed).
narrative_ontology:measurement(gold_be_t1985, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1985, 0.78).
narrative_ontology:measurement_basis(gold_be_t1985, observed).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1958, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1958, 0.25).
narrative_ontology:measurement_basis(gold_su_t1958, observed).
narrative_ontology:measurement(gold_su_t1965, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1965, 0.4).
narrative_ontology:measurement_basis(gold_su_t1965, observed).
narrative_ontology:measurement(gold_su_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1971, 0.6).
narrative_ontology:measurement_basis(gold_su_t1971, observed).
narrative_ontology:measurement(gold_su_t1975, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1975, 0.58).
narrative_ontology:measurement_basis(gold_su_t1975, observed).
narrative_ontology:measurement(gold_su_t1980, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement_basis(gold_su_t1980, observed).
narrative_ontology:measurement(gold_su_t1985, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1985, 0.62).
narrative_ontology:measurement_basis(gold_su_t1985, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__creditor_discipline_reading, resource_allocation).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, composite_overdetermination_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, eurodollar_market_expansion).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, petrodollar_recycling_system).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language label 'the gold-fiat transition' / 'Nixon Shock', per the ε-invariance principle. The automatic_constraint_reading and composite_overdetermination_reading are separate files with their own ε, beneficiaries, and claimed_type, describing the same historical episode through structurally distinct lenses (material-constraint substitution; overdetermined convergence). All three share the kernel_id gold_fiat_transition_mechanism and are cross-linked via affects_constraints. This reading's ε (0.78) is substantially higher than what the automatic_constraint_reading would author, because this reading foregrounds a specific distributive winner/loser structure the other readings treat as secondary or absent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
