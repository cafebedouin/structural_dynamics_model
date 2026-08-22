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
 *   constraint_id: gold_fiat_transition_mechanism__creditor_discipline_reading
 *   human_readable: Bretton Woods to Fiat Transition: Creditor Discipline Mechanism Elimination
 *   domain: monetary_economics/political_economy
 *
 * SUMMARY:
 *   The Bretton Woods system (1944–1971) anchored all currencies to the US
 *   dollar at fixed rates, and the dollar to gold at $35 per ounce. Nations
 *   accumulating dollar reserves could redeem them for gold, creating a
 *   creditor discipline mechanism: if a debtor nation (especially the US
 *   itself) ran persistent deficits, creditor nations could threaten or
 *   execute gold redemptions, forcing balance-of-payments correction. Nixon's
 *   suspension of gold redemption (August 1971) eliminated this mechanism,
 *   freeing the US and other reserve-currency issuers from the external
 *   discipline that had constrained fiscal policy. This reading frames that
 *   transition as a geopolitical power shift: creditor nations (especially
 *   France, Germany, and the UK) lost the veto power they had held over US
 *   monetary and fiscal autonomy; the reserve-currency issuer gained the
 *   ability to run deficits and monetize them without triggering a
 *   balance-of-payments crisis forced by gold runs. The constraint is
 *   understood as HIGH extractive because the discipline elimination enabled
 *   fiscal flexibility and seigniorage capture for the reserve issuer, while
 *   the same transition tightened financial discipline for non-reserve
 *   creditors (they could no longer rely on gold reserves to prevent currency
 *   speculation; they faced new currency-market-based discipline instead).
 *   This reading differs from the automatic_constraint_reading (which frames
 *   the transition as a replacement of physical constraint with institutional
 *   discretion) and the composite_overdetermination_reading (which denies
 *   Nixon Shock as causal, instead attributing the outcome to convergent
 *   structural forces). This is the CREDITOR_DISCIPLINE_READING: the focus is
 *   on who lost leverage, not on what physical constraint was replaced.
 *
 * KEY AGENTS:
 *   - reserve_currency_issuer (US): The primary beneficiary — gains fiscal flexibility, loses gold redemption discipline, can print the reserve asset without balance-of-payments constraint
 *   - creditor_nations (France, Germany, UK): Primary victims — lose the gold redemption threat as leverage over US policy, face currency-market discipline instead
 *   - debtor_nations (non-reserve): Secondary beneficiaries (escape Bretton Woods peg discipline) but also secondary victims (become vulnerable to currency speculation, capital flight)
 *   - central_banks (as collective authority structure): The agenda-setter enforcing or dissolving the redemption mechanism
 *   - financial_markets (currency speculators): The excluded party that would have benefited from open capital flows; Bretton Woods peg maintained their exclusion
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
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__creditor_discipline_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__creditor_discipline_reading, "Bretton Woods to Fiat Transition: Creditor Discipline Mechanism Elimination").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__creditor_discipline_reading, "monetary_economics/political_economy").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__creditor_discipline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__creditor_discipline_reading, '5aa78254-08f4-4801-ad7a-cc3c2424721e').
narrative_ontology:cs_kernel_codification('5aa78254-08f4-4801-ad7a-cc3c2424721e', fixed_text).
narrative_ontology:cs_authority_grounding('5aa78254-08f4-4801-ad7a-cc3c2424721e', extraction).
narrative_ontology:cs_interpretation_layer_present('5aa78254-08f4-4801-ad7a-cc3c2424721e').
narrative_ontology:cs_reading_relation('5aa78254-08f4-4801-ad7a-cc3c2424721e', gold_fiat_transition_mechanism__automatic_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('5aa78254-08f4-4801-ad7a-cc3c2424721e', gold_fiat_transition_mechanism__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('5aa78254-08f4-4801-ad7a-cc3c2424721e', foundational, creditor_redemption_threat_disciplines_reserve_issuer).
narrative_ontology:cs_axiom_status(creditor_redemption_threat_disciplines_reserve_issuer, holdable).
narrative_ontology:cs_axiom_grounding('5aa78254-08f4-4801-ad7a-cc3c2424721e', creditor_redemption_threat_disciplines_reserve_issuer, empirically_contingent).
narrative_ontology:cs_axiom('5aa78254-08f4-4801-ad7a-cc3c2424721e', foundational, creditor_nations_lose_geoeconomic_leverage_post_1971).
narrative_ontology:cs_axiom_status(creditor_nations_lose_geoeconomic_leverage_post_1971, holdable).
narrative_ontology:cs_axiom_grounding('5aa78254-08f4-4801-ad7a-cc3c2424721e', creditor_nations_lose_geoeconomic_leverage_post_1971, empirically_contingent).
narrative_ontology:cs_reference_frame('5aa78254-08f4-4801-ad7a-cc3c2424721e', bretton_woods_creditor_discipline).
narrative_ontology:cs_drift_state('5aa78254-08f4-4801-ad7a-cc3c2424721e', post_nixon_shock_petrodollar_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('5aa78254-08f4-4801-ad7a-cc3c2424721e', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_reserve_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations_non_reserve).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations_non_reserve).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The United States, as the issuer of the dollar and anchor of the Bretton Woods system. Sets the terms of monetary and fiscal policy, holds the gold reserve that backs the dollar, and ultimately makes the decision to suspend redemption. Under Bretton Woods, runs the risk of gold redemptions forcing policy correction. After 1971, gains the ability to run persistent deficits and finance them through capital inflows (Treasury securities held by foreign governments) without triggering a balance-of-payments crisis. The exit option is effectively irrelevant—the US cannot opt out of being the reserve issuer; instead, it captures or loses the extraction depending on whether the redemption mechanism exists.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer, beneficiary).

% France, Germany, the UK, Japan, and other net creditors to the US. Under Bretton Woods, they accumulate dollar reserves and hold the implicit threat of gold redemption, which disciplines US fiscal policy. They experience the transition as loss of leverage: once Nixon closes the gold window, their threat is worthless, and they face a choice between holding dollars at below-market returns (financing US deficits) or diversifying into other currencies/assets (risking currency depreciation, market instability, and geopolitical tension with the US). Their exit is technically possible (they could refuse to hold dollars) but practically constrained (there is no alternative asset of sufficient scale and safety; refusing dollars risks economic isolation).
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations, payer,
    institutional, generational, constrained, global).

% Developing nations and non-reserve debtor nations. Under Bretton Woods, they are constrained to fixed exchange rates against the dollar, which limits their fiscal flexibility and forces balance-of-payments adjustment through deflation or capital inflows. After 1971, they gain the theoretical freedom to float their currencies, set independent monetary policy, and run deficits (financed by capital inflows). But they immediately face currency-market discipline: speculation, capital flight, and high interest rates to defend currency become the new constraining mechanism. Their exit is more mobile than creditor nations (they can dollarize, default, or float) but at the cost of reduced capital inflows and higher borrowing costs.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations_non_reserve, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations_non_reserve, payer).

% Private and institutional holders of gold (central banks, private investors, corporations). Under Bretton Woods, their gold holdings are implicitly backstopped by the US commitment to maintain the $35-per-ounce peg. After the suspension, the peg collapses, and the gold price floats. Holders with short positions or expectations of continued pegging lose wealth; holders with long positions gain. The key extraction is the destruction of the fixed-price option value—the certainty that gold is redeemable at $35/oz becomes worthless. They are trapped because reversing the transition would require re-establishing the Bretton Woods regime, which is economically and politically infeasible.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_reserve_holders, payer,
    moderate, biographical, trapped, global).

% The International Monetary Fund (proposing Special Drawing Rights), the European Commission (later proposing the euro), and academic economists (proposing alternative reserve arrangements). They would have designed a multilateral reserve asset not tied to any single nation's currency, eliminating the reserve-issuer's exorbitant privilege and the creditor-nations' discipline loss. They are excluded because the reserve-currency issuer (US) has sufficient geopolitical power to suppress alternatives and because existing dollar-holder incentives favor maintaining the status quo (no better alternative exists). Their exclusion is the suppression mechanism: the reserve issuer prevents the emergence of competitive reserve assets through policy (US opposition to SDR expansion) and geopolitical leverage (dollar dominance reinforced by military/political power).
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, alternative_reserve_asset_proponents, excluded,
    powerful, generational, constrained, global).

% Financial traders and capital-flow managers. Under Bretton Woods fixed rates, they are largely excluded from profitable arbitrage (rates are pegged, capital flows are controlled). After 1971, they become the NEW ENFORCEMENT MECHANISM for discipline: they can short currencies, force devaluations, and impose discipline through speculation that Bretton Woods creditor nations could never achieve through gold redemption. Their exclusion under Bretton Woods is the cost of the fixed-peg system; their inclusion after 1971 is both a liberation (new profit opportunities) and a new source of discipline for debtor nations.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, currency_speculators, excluded,
    powerful, immediate, arbitrage, global).

% The collective of central banks (Federal Reserve, Banque de France, Bundesbank, etc.) that collectively decide whether to defend the Bretton Woods peg. As gold reserves deplete and capital flows pressure the peg, they face a choice: collectively restore credibility (via massive intervention and policy coordination), or allow the system to collapse. They choose collapse—by refusing to intervene massively in 1971, they ratify Nixon's unilateral decision to suspend redemption. Their analytical stance reflects that they see the system as having become untenable; the transition is treated as inevitable, not engineered.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, central_banking_community, agenda_setter,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__creditor_discipline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stable exchange rates and a single trusted reserve asset eliminate currency chaos, reduce hedging costs, and enable confidence in international capital flows. The Bretton Woods system solves the problem of the 1930s (competitive devaluations, currency instability) by anchoring all currencies to a single asset (gold) and enforcing fixed parities through the commitment to redeem dollars for gold. This is a genuine coordination function—if the peg collapses, nations face uncertainty, hedging costs rise, and capital flows become unstable.
% TRANSFER_FUNCTION: Moves the benefit of seigniorage and capital inflows from the global system to the reserve-currency issuer (US). Under Bretton Woods, the US gains seigniorage (prints dollars backed by gold, issues them at face value) but loses credibility if the redemption threat is executed (gold runs drain reserves, forcing policy correction). After 1971, the US captures full seigniorage and can finance deficits through capital inflows (foreign governments and central banks hold dollars to solve their own reserve needs, funding US fiscal policy at below-market interest rates). Creditor nations transfer their accumulated dollar reserves into a below-market return asset, financing US deficits. The transfer is from creditor nations to the US reserve issuer, mediated by the elimination of the gold redemption threat.
% ABSENT_VOICES: Alternative-reserve-asset proponents and currency speculators would object if invited to the original Bretton Woods design. The alternative proponents would argue for a multilateral reserve asset (SDR, etc.) that eliminates the reserve-issuer's exorbitant privilege. Currency speculators would argue for floating rates that allow arbitrage and eliminate the fixed-peg constraint. Both groups are structurally excluded under Bretton Woods: the reserve issuer (US) suppresses alternatives (opposes SDR expansion), and the fixed-peg system removes profitable speculation opportunities. After 1971, speculators enter the picture as the new enforcement mechanism (currency market discipline), while the alternative-reserve proponents remain excluded (the dollar re-establishes its dominance through geopolitical leverage and the petrodollar system).
% DISAPPEARANCE_RATIONALE: If the gold-redemption discipline mechanism had persisted (or if it were restored), the US would face severe balance-of-payments constraints. Persistent deficits would trigger gold runs; creditor nations could execute redemptions to drain US gold reserves, forcing massive policy correction (either through deflation, capital controls, or currency devaluation). This would have prevented the massive deficits and capital outflows that characterized US policy from 1971 onward. The post-Bretton Woods world (petrodollar recycling, US Treasury dominance, currency-market discipline replacing gold redemption) would not have emerged. Instead, either a reformed Bretton Woods peg (with expanded gold supplies or a different anchor asset) would have persisted, or nations would have floated to market-clearing exchange rates earlier (without the reserve-issuer's deficit financing).
% FOUNDING_PROBLEM: The gold-backed Bretton Woods system was designed to solve two problems: (1) eliminate the competitive devaluations and currency chaos of the 1930s (the stated founding problem), and (2) discipline the reserve-currency issuer by imposing a hard constraint on deficit spending (the implicit founding problem for creditor nations). By the 1960s, problem (1) had been solved—exchange rates were stable, currency speculation was minimal. But problem (2) became contested: the US began running persistent deficits (Vietnam War, Great Society spending), and creditor nations began to question whether continued dollar accumulation served their interests. The Triffin dilemma captured the tension: if the US ran deficits to supply the world with needed reserve currency, eventually creditors would accumulate so many dollars that redemption threats would become credible, forcing correction; but if the US ran surpluses to prevent inflation and maintain gold backing, the world would face insufficient reserve liquidity.
% FOUNDING_PROBLEM_CORROBORATION: The finding problem is contested by all major parties. The US and academic economists sympathetic to the reserve-issuer argued that problem (1) was solved and the system should be reformed to allow larger capital flows and independent monetary policy (this group supported floating rates or a reformed Bretton Woods). European creditor nations argued that problem (2) remained live—the US was using its reserve-issuer privilege to run deficits at their expense, and the discipline mechanism should be strengthened (France and Germany advocated for alternative reserves or reformed pegging). International institutions (IMF, World Bank) documented both problems in contemporaneous reports and testimony. Non-aligned nations and academic development economists argued the system imposed discipline on all debtor nations and should be replaced with a truly multilateral reserve system. The consensus outside the US beneficiary seat was that the system's founding problem (currency chaos) had been solved, but its hidden purpose (reserve-issuer discipline via gold redemption) had become the real battleground. Scholarly sources outside the benefiting party: Triffin (1960), Kindleberger (1993), Steil (2013)—all confirm the shift from founding problem solved to mandate obsolete but extraction persisting.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__creditor_discipline_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__creditor_discipline_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__creditor_discipline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.45 to 0.78 across the interval (t=0 Bretton Woods to t=20 post-Petrodollar regime). Early extractiveness is moderate because the creditor-discipline mechanism was still in force: yes, the US benefited from seigniorage, but the gold redemption threat constrained it. At t=8 (Nixon Shock), extractiveness jumps sharply to 0.71 as the redemption threat evaporates. At t=12–20, extractiveness stabilizes at 0.78 as the Petrodollar system (US deficits financed by oil-producer recycled petrodollars and recycled by US Treasury securities) replaces gold redemption as the mechanism for deficit financing—the extraction is now purely institutional, no longer constrained by a material redemption right. Suppression is moderate-high (0.38→0.62) because maintaining the system requires active enforcement: the US must deter creditor-nation currency diversification away from dollars, must maintain geopolitical dominance sufficient to make dollar hegemony credible, must suppress the emergence of alternative reserve assets (this is why the US blocked SDR expansion and later promoted petrodollar recycling). Theater is low-to-moderate (0.12→0.28) because the real function (enabling reserve-issuer fiscal autonomy) is straightforward; the performative layer is limited to the justification narratives (US Treasuries are safe, the dollar is stable, etc.) that maintain creditor-nation willingness to hold dollar reserves. The measured suppression is substantially lower than extractiveness because the system is not held together by force—it rests on the structural incentive for creditor nations to keep dollar reserves despite the discipline loss (they have no better alternative for capital deployment). Accessibility collapse is moderate (0.71) because creditor nations retain a theoretical exit (shift to an alternative reserve asset), but that exit is practically unavailable: no alternative asset had the scale, safety, or network liquidity of dollars in 1971. Resistance is moderate-high (0.55) because creditor nations resisted—France conducted a gold run in the 1960s, the Triffin dilemma was widely debated, there was serious discussion of alternative reserve arrangements—but the resistance failed because the reserve-issuer's dominance was too asymmetric.
 *
 * PERSPECTIVAL GAP:
 *   From the reserve-issuer's seat (the US), the transition is a liberation: the constraint is understood as 'we gained fiscal autonomy and the ability to finance deficits through capital inflows rather than gold redemption.' From the creditor-nation seat (France, Germany), the same transition is a capture: 'we lost leverage over US policy and are now locked into dollar accumulation by the logic of capital markets.' The engine computes these as divergent directionality positions (d ≈ 0.2 for US, d ≈ 0.85 for France/Germany), which should produce different per-seat type classifications: the US might compute as snare-beneficiary or rope-beneficiary (depending on whether the suppression of alternatives is treated as part of the constraint), while France/Germany compute as snare-target (high extraction from holding unwanted dollars, no beneficial coordination function for them personally, constrained exit). The analytical seat—a central banker or economist observing the system—might classify the whole arrangement as a Tangled Rope: genuine coordination function (a stable reserve asset solves a real problem), but with extraction benefiting the issuer and victims among the dollar-hoarders. This perspectival divergence is not a flaw; it is exactly the signal the engine measures.
 *
 * DIRECTIONALITY LOGIC:
 *   The reserve_currency_issuer (US) is the primary beneficiary: it gains fiscal flexibility (can run deficits without gold-redemption discipline), retains seigniorage (the ability to mint the reserve asset), and captures the extraction through interest income on Treasury securities held by foreign governments and central banks. From this agent's position, d ≈ 0.2 (net beneficiary, though facing some currency-stability constraint). Creditor_nations (France, Germany, UK) are the primary victims: they lose the gold redemption leverage they had held over US monetary policy, must hold dollar reserves despite the discipline loss, and face the new currency-market-based discipline (speculation, capital flight risk) that the post-Bretton Woods regime introduced. From their position, d ≈ 0.85 (net targets, though they retain some choice in alternative assets and the choice to hold dollars). Debtor_nations (non-reserve issuers) experience the transition as mixed: they escape the Bretton Woods peg (which constrained them to fixed dollar exchange rates), so they gain short-term fiscal flexibility, but they immediately become vulnerable to currency-market discipline and capital flight (higher interest rates to defend currency). Their d is approximately 0.5 (symmetric: coordination benefit from reserve-asset existence, extraction cost from currency-market discipline). The analytical seat (competition authorities, academic economists, central bankers studying the transition) is observer. Suppression is higher for creditor nations than for non-reserve debtors: creditor nations cannot diversify away from dollars without losing geopolitical influence and market access; the US military and political dominance constrains their alternatives. Non-reserve debtors retain some exit (they can default on their debt, can dollarize—there is less suppression of their choice set). No directionality override is needed; the derived d values map cleanly from the beneficiary/victim declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was solved (in the automatic_constraint_reading framing): Bretton Woods was built to create predictable exchange rates and prevent the competitive devaluations of the 1930s. It succeeded: exchange rates were stable from 1944–1971. But its mandate was becoming obsolete by the 1960s—capital flows had grown larger than the fixed gold stock could support (the Triffin dilemma), and the original problem (1930s currency chaos) was no longer the driver of policy. In the creditor_discipline_reading framing, the founding problem was DIFFERENT: the constraint existed to discipline US deficits via the gold redemption threat. By that standard, the founding problem became politically CONTESTED in the 1960s (France questioned it publicly; US policymakers denied it existed) and was formally superseded in 1971 (Nixon closed the gold window). The base_properties.mandatrophy_resolved field should be TRUE: the Bretton Woods system was functionally obsolete by 1971, its founding mandate had shifted from 'prevent currency chaos' to 'discipline the reserve issuer,' and that shifted mandate was actively rejected. But the system persisted for several years after collapse in an informal fashion (dollar hegemony, petrodollar recycling), suggesting zombie institutional inertia. This reading resolves mandatrophy by acknowledging that the discipline function was the contested core, not the chaos-prevention function; once the discipline was eliminated, the system could no longer claim a mandate and devolved into pure extraction (dollar hegemony maintained by geopolitical dominance, not by any legitimate coordination function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_automatic_constraint,
    'Is the transition best understood as elimination of creditor disciplinary power (this reading''s frame) or as replacement of a material constraint with an institutional discretion regime (the automatic_constraint_reading)?',
    'Historical analysis of causal narratives in policy testimony and scholarly reconstruction: did policymakers frame the transition as restoring freedom from creditor pressure (creditor_discipline framing) or as replacing physical limits with central bank authority (automatic_constraint framing)? Do the two framings classify the same constraint or different constraints?',
    'If this reading is correct, the constraint is high-ε extraction eliminating a balance-of-payments discipline mechanism; the beneficiary is the reserve issuer freed from discipline, the victim is the creditor nations losing veto leverage. If the automatic_constraint reading is correct, the constraint is a type change from material to institutional (different ε values, different victim structure). The framings may name different constraints entirely (ε-invariance test).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_automatic_constraint, conceptual, 'Whether the transition eliminated creditor veto power or replaced a material constraint with discretionary institutional authority.').

omega_variable(
    composite_overdetermination_vs_single_mechanism,
    'Did the creditor-discipline mechanism actually provide balance-of-payments discipline, or was the transition driven by multiple independent structural changes (telecommunications, capital flows, labor dynamics, legal tender enforcement) that happened to coincide at Nixon Shock?',
    'Counterfactual analysis: if Nixon had not closed the gold window in 1971, would creditor-nation discipline have persisted (proving the mechanism was causal) or would other forces (capital mobility, wage pressure, technology) have forced the same outcomes independently?',
    'If the mechanism was causal, this reading is structurally sound and the transition eliminated a real creditor veto. If overdetermined, the mechanism was already decaying; the reading misattributes agency to a constraint that was already becoming obsolete. The constraint''s claimed_type and ε remain stable under this omega (both readings agree something changed), but the interpretation of what changed and why differs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(composite_overdetermination_vs_single_mechanism, empirical, 'Whether creditor discipline was a causal mechanism or a symptom of broader structural change.').

omega_variable(
    reserve_currency_exceptionalism,
    'Did the transition eliminate creditor discipline uniformly for all nations, or only for the reserve-currency issuer (the US)?',
    'Comparative historical analysis: trace balance-of-payments discipline for reserve-issuer (US) versus non-reserve debtor nations (UK, France, smaller debtors) through and after the transition. Do non-reserve nations retain balance-of-payments vulnerability to creditor pressure (through exchange-rate speculation, capital flight, foreign-exchange reserves requirements) while the reserve issuer escapes it?',
    'If uniform elimination: this reading describes a global regime change with all debtor nations as beneficiaries. If selective: the constraint is tightened for non-reserve debtors (they remain disciplined via currency markets) while eliminated for the reserve issuer (it can print the reserve asset). The reading''s ε and victim/beneficiary structure remain correct, but the spatial scope and directionality differentiation becomes sharper—non-reserve debtors may be victims of a different, market-based discipline replacing gold redemption.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reserve_currency_exceptionalism, empirical, 'Whether the creditor-discipline elimination applied to all debtor nations or only reserve-currency issuers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__creditor_discipline_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(creditor_discipline_theater_t0_bretton_woods, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(creditor_discipline_theater_t0_bretton_woods, observed).
narrative_ontology:measurement(creditor_discipline_theater_t4_late_sixties, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement_basis(creditor_discipline_theater_t4_late_sixties, observed).
narrative_ontology:measurement(creditor_discipline_theater_t8_nixon_shock, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement_basis(creditor_discipline_theater_t8_nixon_shock, observed).
narrative_ontology:measurement(creditor_discipline_theater_t12_post_collapse, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement_basis(creditor_discipline_theater_t12_post_collapse, observed).
narrative_ontology:measurement(creditor_discipline_theater_t16_petrodollar, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement_basis(creditor_discipline_theater_t16_petrodollar, observed).
narrative_ontology:measurement(creditor_discipline_theater_t20_stabilized, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(creditor_discipline_theater_t20_stabilized, observed).

% Extraction over time
narrative_ontology:measurement(creditor_discipline_extraction_t0_bretton_woods, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(creditor_discipline_extraction_t0_bretton_woods, observed).
narrative_ontology:measurement(creditor_discipline_extraction_t4_late_sixties, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 4, 0.58).
narrative_ontology:measurement_basis(creditor_discipline_extraction_t4_late_sixties, observed).
narrative_ontology:measurement(creditor_discipline_extraction_t8_nixon_shock, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 8, 0.71).
narrative_ontology:measurement_basis(creditor_discipline_extraction_t8_nixon_shock, observed).
narrative_ontology:measurement(creditor_discipline_extraction_t12_post_collapse, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 12, 0.76).
narrative_ontology:measurement_basis(creditor_discipline_extraction_t12_post_collapse, observed).
narrative_ontology:measurement(creditor_discipline_extraction_t16_petrodollar, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 16, 0.78).
narrative_ontology:measurement_basis(creditor_discipline_extraction_t16_petrodollar, observed).
narrative_ontology:measurement(creditor_discipline_extraction_t20_stabilized, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement_basis(creditor_discipline_extraction_t20_stabilized, observed).

% Suppression requirement over time
narrative_ontology:measurement(creditor_discipline_suppression_t0_bretton_woods, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(creditor_discipline_suppression_t0_bretton_woods, observed).
narrative_ontology:measurement(creditor_discipline_suppression_t4_late_sixties, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 4, 0.48).
narrative_ontology:measurement_basis(creditor_discipline_suppression_t4_late_sixties, observed).
narrative_ontology:measurement(creditor_discipline_suppression_t8_nixon_shock, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 8, 0.61).
narrative_ontology:measurement_basis(creditor_discipline_suppression_t8_nixon_shock, observed).
narrative_ontology:measurement(creditor_discipline_suppression_t12_post_collapse, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement_basis(creditor_discipline_suppression_t12_post_collapse, observed).
narrative_ontology:measurement(creditor_discipline_suppression_t16_petrodollar, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement_basis(creditor_discipline_suppression_t16_petrodollar, observed).
narrative_ontology:measurement(creditor_discipline_suppression_t20_stabilized, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(creditor_discipline_suppression_t20_stabilized, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__creditor_discipline_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.18).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism__automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism__composite_overdetermination_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, bretton_woods_fixed_exchange_rate_peg).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, petrodollar_recycling_regime).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_seigniorage_extraction).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the gold-fiat transition kernel. The automatic_constraint_reading frames the same historical event as a replacement of material constraint with institutional discretion (different ε, different victim/beneficiary structure). The composite_overdetermination_reading denies the transition was caused by the discipline mechanism—it was overdetermined by capital mobility and labor dynamics. All three readings share the kernel (Bretton Woods system and its dissolution) but instantiate different constraints with different ε values and different structural interpretations. Network links document the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
