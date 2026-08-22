% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_composite_overdetermination, []).

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
 *   constraint_id: gold_fiat_transition_mechanism__composite_overdetermination_reading
 *   human_readable: Gold-to-Fiat Transition as Composite Structural Convergence (Overdetermination Reading)
 *   domain: monetary_economics/political_economy
 *
 * SUMMARY:
 *   The transition from gold-backed (Bretton Woods) to floating-rate
 *   (post-1971) monetary order is commonly understood as a single causal
 *   pivot—typically attributed either to the exhaustion of physical gold
 *   constraints or to a US geopolitical bid to escape creditor discipline.
 *   This reading rejects the singular-cause framing. Instead, it identifies
 *   the transition as convergence of multiple independent structural changes
 *   that happened to align in the early 1970s: (1) telecommunications
 *   technology enabling instant capital flows across borders; (2) the
 *   collapse of the Bretton Woods peg, driven by US deficits and gold reserve
 *   depletion; (3) labor bargaining-power shifts in surplus countries,
 *   weakening domestic acceptance of import-driven wage erosion; (4)
 *   maturation of legal-tender enforcement mechanisms (central bank
 *   credibility, domestic taxation systems) making fiat currency acceptable
 *   without commodity backing. Nixon Shock (15 August 1971) was a political
 *   marker and decision point, but not the causal node. The structural
 *   overdetermination means that had Nixon not suspended convertibility, the
 *   peg would have collapsed within months via market pressure anyway—the
 *   multiple independent forces made the transition inevitable, not
 *   contingent on one decision.
 *
 * KEY AGENTS:
 *   - Reserve-currency issuer (US): gains monetary autonomy; loses gold-reserve discipline; sets the post-transition monetary regime.
 *   - Creditor nations (France, Germany, Japan): lose redemption veto; holdings become claims on unanchored currency; must adapt to floating volatility.
 *   - Peripheral non-reserve economies: gain theoretical monetary independence; lose external constraint as stability anchor; face new capital-flow volatility.
 *   - Labor in surplus countries: lose wage-stabilization benefit of fixed exchange rates; gain from potential full-employment policies if deployed domestically.
 *   - Labor in reserve-currency issuer: temporarily benefit from monetary expansion; harmed by subsequent inflation; bargaining power eventually pressures wages.
 *   - Telecommunications infrastructure operators: directly enabled by the transition; benefit from real-time settlement demand.
 *   - Financial market actors: extract volatility premiums from floating rates; gain from carry-trade opportunities; capture rents from exchange-rate arbitrage.
 *   - Bretton Woods administrators (IMF, World Bank): face institutional obsolescence; must redeploy into new mandates (structural adjustment, development lending).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.58).
domain_priors:suppression_score(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.42).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__composite_overdetermination_reading, "Gold-to-Fiat Transition as Composite Structural Convergence (Overdetermination Reading)").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__composite_overdetermination_reading, "monetary_economics/political_economy").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__composite_overdetermination_reading, '53699ccd-1f49-46e3-9f24-0227592993de').
narrative_ontology:cs_kernel_codification('53699ccd-1f49-46e3-9f24-0227592993de', distributed).
narrative_ontology:cs_authority_grounding('53699ccd-1f49-46e3-9f24-0227592993de', extraction).
narrative_ontology:cs_reading_relation('53699ccd-1f49-46e3-9f24-0227592993de', gold_fiat_transition_mechanism__automatic_constraint_reading, influences).
narrative_ontology:cs_reading_relation('53699ccd-1f49-46e3-9f24-0227592993de', gold_fiat_transition_mechanism__creditor_discipline_reading, influences).
narrative_ontology:cs_axiom('53699ccd-1f49-46e3-9f24-0227592993de', foundational, transition_structurally_overdetermined).
narrative_ontology:cs_axiom_status(transition_structurally_overdetermined, holdable).
narrative_ontology:cs_axiom_grounding('53699ccd-1f49-46e3-9f24-0227592993de', transition_structurally_overdetermined, empirically_contingent).
narrative_ontology:cs_axiom('53699ccd-1f49-46e3-9f24-0227592993de', secondary, technological_infrastructure_enabling_real_time_settlement).
narrative_ontology:cs_axiom_status(technological_infrastructure_enabling_real_time_settlement, holdable).
narrative_ontology:cs_axiom_grounding('53699ccd-1f49-46e3-9f24-0227592993de', technological_infrastructure_enabling_real_time_settlement, empirically_contingent).
narrative_ontology:cs_reference_frame('53699ccd-1f49-46e3-9f24-0227592993de', bretton_woods_parity_discipline).
narrative_ontology:cs_drift_state('53699ccd-1f49-46e3-9f24-0227592993de', post_1971_float_adoption, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('53699ccd-1f49-46e3-9f24-0227592993de', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuers).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, debtors_with_hard_currency_exposure).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, creditors_dependent_on_redemption).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, peripheral_economies_without_reserve_status).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, labor_in_surplus_country).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, labor_in_reserve_currency_issuer).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, debtor_nation_with_hard_currency_exposure).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, telecommunications_infrastructure_operators).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, financial_market_actors).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, creditor_nation).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, peripheral_economy_non_reserve_issuer).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, labor_in_surplus_country).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, labor_in_reserve_currency_issuer).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, bretton_woods_institutional_administrators).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__composite_overdetermination_reading, multi_causal_institutional_change).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__composite_overdetermination_reading, technological_infrastructure_as_structural_constraint).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The United States, issuer of the reserve currency, experiences the transition as expansion of monetary policy autonomy. Under Bretton Woods, the US could not expand money supply beyond gold reserves without triggering redemption crises; post-transition, monetary expansion becomes possible and inflation becomes the adjustment mechanism instead of gold loss. Controls the Federal Reserve and Treasury, which together set the post-transition monetary regime. Captures the primary extractive benefit through seigniorage and monetary discretion.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuer, agenda_setter,
    institutional, generational, arbitrage, global).

% Nations holding reserve balances and gold claims (France, Germany, other surplus countries) lose the disciplinary veto: under Bretton Woods, they could demand gold and force adjustment; post-transition, their dollar holdings become claims on an unanchored currency. They bear the cost of the US monetary expansion as dollar devaluation and reserve losses. Their attempts to exit (repatriate gold, diversify reserves) are each inadequate: gold repatriation into floating markets makes them as volatile as dollars, and alternative reserves (SDR, other currencies) lack depth. Structurally forced to absorb the extraction.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, creditor_nation, payer,
    institutional, generational, constrained, global).

% Economies without reserve-currency status (most developing nations) experience the transition as loss of the gold standard's apparent objectivity and stability without compensating policy autonomy. Under Bretton Woods, they had a fixed anchor (gold/dollar peg) but limited monetary independence. Post-transition, they gain monetary independence in principle but face new exchange-rate volatility they do not control. Their currencies float against major currencies; their monetary policy remains constrained by capital flows. They lose the external anchor without gaining the power of the issuer.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, peripheral_economy_non_reserve_issuer, payer,
    moderate, biographical, constrained, national).

% Workers in creditor surplus nations (Germany, Switzerland, Japan) initially lose from the transition: their wages (set in domestic currency) lose purchasing power as currencies float and the dollar depreciates. The transition exports inflation to them as the US expands money supply. However, they also gain potential bargaining power: domestic governments, freed from the Bretton Woods constraint, can pursue full-employment policies and monetary expansion to support wages. Whether they gain or lose depends on domestic policy choices made after the transition. They are fundamentally ambiguous stakeholders: cost-bearing payers in the near term, but potential beneficiaries if governments deploy the newfound monetary space for wage support.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, labor_in_surplus_country, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__composite_overdetermination_reading, labor_in_surplus_country, beneficiary).

% US workers experience the early post-transition period as wage growth and near-full employment (1971–1973): monetary expansion supports employment. But the same expansion drives inflation (1973–1980), which erodes real wages. The transition initially benefits them (more jobs, nominal wage growth); the inflation that follows extraction the benefit. They are partly beneficiary (initial monetary expansion supports employment) and partly payer (inflation reduces purchasing power). Their bargaining position initially improves (tight labor markets), then weakens as inflation expectations entrench and central banks tighten policy to restore price stability.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, labor_in_reserve_currency_issuer, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__composite_overdetermination_reading, labor_in_reserve_currency_issuer, payer).

% Nations carrying external debt denominated in reserve currency (most developing and emerging economies) benefit from the transition: their debt becomes nominally fixed while the currency's real value erodes via inflation. A debtor owing 100 units of dollars can repay with cheaper dollars (post-inflation). The transition removes the redemption-threat discipline that could have forced fiscal adjustment; deficits become manageable through inflation instead. Their nominal debt burden decreases in real terms. They are clear beneficiaries of the elimination of creditor veto power.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, debtor_nation_with_hard_currency_exposure, beneficiary,
    powerful, generational, mobile, global).

% Firms providing financial telecommunications (SWIFT, CHIPS, satellites, fiber optics) enable instant capital flows and real-time settlement post-transition. The floating-rate regime requires continuous price discovery and settlement speed that the Bretton Woods fixed-rate system did not. The transition directly validates their infrastructure and expands demand for their services. They are structural beneficiaries without being political agents—their profit rises directly from the constraint transition, but they did not cause it. The transition would have been impossible without them, but they did not lead it.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, telecommunications_infrastructure_operators, beneficiary,
    organized, generational, mobile, global).

% The IMF and World Bank were designed to administer the pegged-rate system: the IMF provided liquidity in a fixed-rate world, the World Bank financed post-war reconstruction and development under the assumption of stable exchange rates. The transition to floating rates strips their primary raison d'être. The IMF's original function (providing short-term liquidity to defend pegs) becomes obsolete; the World Bank's long-term lending assumptions become uncertain under exchange-rate volatility. They must reinvent themselves (the IMF into structural adjustment and balance-of-payments lending, the World Bank into development policy) to remain relevant. They are initial payers (institutional obsolescence) but adapt by capturing new mandates (adjustment conditionality, structural reform) that allow them to survive and even expand. Their secondary role (agenda-setter for post-transition restructuring) emerges over the 1975–1985 interval.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, bretton_woods_institutional_administrators, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__composite_overdetermination_reading, bretton_woods_institutional_administrators, payer).

% Currency traders, banks, and portfolio managers gain directly from the transition by capturing exchange-rate volatility spreads and carry trades. The Bretton Woods peg enforced narrow bid-ask spreads and limited trading opportunity; the floating regime creates daily fluctuations that traders can profit from. Forward markets and currency options markets (non-existent under the peg) become profitable. They are passive beneficiaries of the structural shift—they did not cause the transition but profit from its distribution of uncertainty. Their interests align with persistent float-driven markets and volatility preservation.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, financial_market_actors, beneficiary,
    institutional, biographical, arbitrage, global).

% Scholars and policy analysts tasked with reconstructing the transition and identifying which causal narratives (single-factor vs. composite) best fit the historical record. They examine contemporaneous documents, policy choices, technological developments, and labor-market data to trace the multiple independent streams (US deficits, French and German labor militancy, telecommunications maturation, IMF institutional framing) that converged to produce the transition. Their analysis informs whether the constraint is understood as singular (one kernel, multiple readings) or composite (multiple constraints, one historical moment). They hold no extractive position but their interpretation of causality shapes how policymakers and historians understand the transition.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, observer_seat_economic_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuer).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The gold standard and Bretton Woods system coordinated international monetary settlement by tying nominal values to a physical substrate (gold) and nominal exchange rates to declared parities, eliminating the need for constant renegotiation of bilateral settlement terms. The transition to floating rates removed that coordination mechanism, shifting the settlement problem to continuously negotiated exchange markets.
% TRANSFER_FUNCTION: The transition moved effective monetary policy authority from a distributed gold-redemption discipline (creditor nations could force adjustment by demanding gold) to centralized discretion (the reserve-currency issuer controls the monetary base and inflation trajectory). It also shifted the cost of adjustment from reserve-currency issuers (whose gold reserves limited expansion) to peripheral economies and creditors (who absorbed inflation and devaluation). Different structural changes extracted different distributions: telecommunications enabled capital concentration, labor bargaining shifts redistributed income shares, and the peg collapse eliminated the creditor veto.
% ABSENT_VOICES: Nations that depended on the gold standard as an external constraint to prevent fiscal excess (some peripheral economies that lacked internal fiscal institutions); labor movements that benefited from Bretton Woods stability but were excluded from monetary policymaking; and the successor order's excluded actors (non-reserve-currency issuers who would later face capital-flow volatility). The transition was negotiated by central banks and treasuries; worker representatives, peripheral governments, and future generations dependent on exchange-rate stability were structurally absent.
% DISAPPEARANCE_RATIONALE: If the transition reversed and the gold peg returned tomorrow, central banks would immediately rebuild reserve holdings in gold, exchange rates would re-stabilize at new parities (or new Bretton Woods negotiation), capital markets would lose their carry-trade profitability, and reserve-currency issuers would face redemption discipline again. The modern floating-rate order is constituted by the absence of the peg; its restoration would force wholesale reorganization of financial markets, monetary policy regimes, and international power structures.
% FOUNDING_PROBLEM: The original Bretton Woods system was built to solve the inter-war currency instability and competitive devaluation that destabilized trade and finance. The founding problem was: how can nations conduct international commerce and finance while maintaining domestic monetary policy autonomy? The Bretton Woods answer: fix exchange rates to gold, establish an institutional backstop (IMF) for liquidity crises, and allow limited adjustments under pressure. This worked from 1944 to the late 1960s but relied on the US to maintain gold parity by controlling its money supply—a constraint that became binding as US deficits accumulated.
% FOUNDING_PROBLEM_CORROBORATION: Historians and economists (Eichengreen, Steil, Helleiner, Rodrik) outside the central banking establishment attest that by the late 1960s, the founding problem had morphed: inter-war instability was no longer the threat; instead, the threat was the incompatibility of Bretton Woods (fixed rates + capital mobility + US monetary autonomy) when the first two were genuinely desired but the third was becoming incompatible with the peg. Giscard d'Estaing and the French government called for the peg's replacement openly in 1965 (Eichengreen 2019, Bretton Woods Unraveled); US administration documents show awareness that the system was unsustainable by 1968 (the Gold Pool collapse). The founding problem—inter-war instability—had been solved; the system persisted not because it solved a live problem but because it served particular interests (US monetary flexibility, creditor confidence, institutional legitimacy). By 1971, the founding problem was gone; the constraint persisted as institutional theater.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__composite_overdetermination_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness trajectory (0.35 → 0.62 → 0.58) reflects the overdetermined nature: from 1950–1968, the system extracted modestly because the Bretton Woods coordination was largely functional (trade recovered, economies grew, the peg held). From 1968–1971, extractiveness rose sharply as the system began to break down (gold drain accelerated, creditors demanded conversion, peripheral economies faced impossible discipline). The peak extractiveness at 1971 (0.62) marks the moment of maximum distributional conflict—the peg was collapsing while still nominally enforced, forcing adjustment on multiple parties simultaneously. Post-1971, extractiveness declines to 0.58 as the new floating regime stabilizes and different countries adapt to their new positions (reserve-issuers settle in to discretion, creditors write off losses, peripherals face new volatility but without the contradictory peg constraints). Theater_ratio rises from 1950–1971 (0.12 → 0.31) as the peg becomes increasingly a maintained fiction despite underlying economic reality, then stabilizes post-transition. Suppression_requirement rises 1950–1971 (0.28 → 0.48) as the system requires more active enforcement to maintain the peg against capital flows and reserve loss, then declines slightly post-1975 as the new floating regime's rules become accepted. The shared time grid (1950, 1960, 1968, 1971, 1977, 1985) allows comparison of metric divergence across the transition point.
 *
 * PERSPECTIVAL GAP:
 *   The reserve-currency issuer and the creditor nations compute fundamentally different constraint types from the same structural data. The issuer experiences the transition as rope: the monetary coordination function is preserved (settlement still happens; inflation becomes the adjustment mechanism instead of gold loss), and the issuer captures most of the extracted value. Creditor nations experience it as snare: they are forced to absorb the cost (devaluation of their reserves, loss of redemption discipline) without compensation, and their exit options (holding gold or other currencies) are each inadequate. Peripheral economies experience it as tangled_rope-to-snare: the new monetary regime coordinates global settlement but extracts unpredictably via capital flows and exchange-rate volatility. Labor's experience depends on domestic policy response, not on the global constraint itself. The engine should compute per-seat divergence; the authored claim (tangled_rope) represents the constraint's structural type at the global level, where genuine coordination persists but is paired with asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply by agent. Reserve-currency issuer: d ≈ 0.25 (full beneficiary—the entire transition expands their policy space). Creditor nations: d ≈ 0.85 (near-full target—they absorb the peg's collapse, lose their veto, hold devalued reserves). Peripheral economies: d ≈ 0.65 (target, but with some flexibility in new regime). Labor in surplus countries: d ≈ 0.55 (symmetric—lose fixed-rate stability, gain autonomy for domestic policy; outcome depends on policy choice). Labor in reserve-issuer: d ≈ 0.50–0.70 (initially beneficiary from monetary expansion, then target from inflation). Telecommunications operators: d ≈ 0.1 (beneficiary—the regime change validates and expands their infrastructure demand). Financial markets: d ≈ 0.15 (beneficiary—volatility is profitable). Bretton Woods administrators: d ≈ 0.75 (target—their raison d'être disappears). The high directionality variance across the agent set reflects that this is a composite over-determined transition: different structural changes extract from and benefit different agents, and no single consistent directionality vector applies. This is exactly the reading's claim against the singular-cause framings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (inter-war instability) was dead by 1970—the system persisted not to solve it but because it served institutional interests (US flexibility, creditor confidence, bureaucratic legitimacy). By 1968, the Gold Pool had broken, the US was running persistent deficits, and central bankers were openly discussing the system's unsustainability. The theater_ratio's rise from 0.12 (1950) to 0.31 (1971) documents the growing gap between the system's nominal function (maintain pegged rates and discipline) and its actual operation (managed adjustment through controls, special arrangements, institutional theater). The suppression_requirement's rise (0.28 → 0.48) shows that maintaining the peg required increasingly active enforcement: capital controls, coordinated central-bank intervention, the London Gold Pool. The base_extractiveness surge (0.48 → 0.62) between 1968–1971 marks the moment when the constraint's extractive character became undeniable (creditors and peripherals faced impossible discipline, the issuer was forced to choose between adjustment and default). By the six_questions verdict, the founding problem is dead and the disappearance_verdict is world_rearranges: the constraint did not fail because it was bad, but because its function was gone and maintaining it required theater beyond credibility. The transition was inevitable not because it was optimal but because multiple independent forces made the system's collapse overdetermined.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_as_constraint_vs_enabler,
    'Did telecommunications technology enable the transition (by making real-time capital flows and float-management possible) or was it merely compatible with a transition driven by other forces?',
    'Counterfactual: if telecommunications had remained limited to pre-1960 capabilities (telex, delayed settlement), could the Bretton Woods system have persisted despite US deficits? Historical analysis of central bank strategies for float-management—if they uniformly attest that real-time data and instantaneous settlement were necessary for managing volatility, the technology was a binding constraint, not an enabler.',
    'If technology was a binding constraint, the transition''s timing is largely technological (exogenous to policy and labor). If merely compatible, technology had lower causal weight and policy/labor factors dominate. The measured extractiveness would be stable across different technology regimes if policy-driven, or contingent on technology regime if technology is dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_as_constraint_vs_enabler, empirical, 'Whether real-time settlement capability was necessary for floating-rate regime viability.').

omega_variable(
    labor_bargaining_power_causality,
    'Did rising labor bargaining power in surplus countries force the Bretton Woods peg''s collapse by making external constraints unacceptable? Or did labor-power shifts follow the peg''s collapse and reflect adjustment to new floating volatility?',
    'Temporal sequence: wage-push inflation and labor militancy in Germany, France, and Japan in 1968–1970 (before peg collapse) vs. 1975–1980 (after), weighted by contemporaneous labor testimony about constraint acceptance. If labor militancy pre-dates the peg collapse and was vocal about the peg''s constraints, causality runs from labor to transition. If militancy post-dates peg collapse, causality runs the other way.',
    'If labor causality is confirmed, the transition is partly endogenous to distributional conflict (not purely structural/technological). This would support a reading where the composite transition includes genuine political contestation, not just structural inevitability. Extractiveness interpretation shifts: if labor-driven, the transition is a contested redistribution; if exogenous, it is a structural realignment imposed on labor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_bargaining_power_causality, empirical, 'Whether labor bargaining power shifts causally drove or post-dated the peg''s collapse.').

omega_variable(
    kernel_singularity_itself,
    'Is the gold-fiat transition best understood as a single structural event (one kernel with multiple readings) or as a cluster of distinct transitions (multiple kernels that happened to occur together)?',
    'Linguistic/conceptual: if the transition is describable as a single narrative with coherent beneficiaries and victims across the process (e.g., ''the reserve-currency issuer''s monetary power expanded''), then it is a single kernel with multiple readings. If the transition requires separate narratives for separate agent groups (labor narrative, technology narrative, creditor narrative) that do not cohere, it is multiple kernels coincidentally aligned.',
    'If a single kernel: the constraint story''s claim remains tangled_rope (one global constraint with asymmetric distribution). If multiple kernels: this story is mis-framed as one constraint; it should decompose into separate stories (one per structural driver: technology_enables_capital_flows, labor_extracts_from_pegged_discipline, creditor_loses_veto, etc.). The engine would then compute per-kernel classifications rather than a single global type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_singularity_itself, conceptual, 'Whether the transition is a single multi-faceted constraint or multiple distinct constraints that coincidentally align in time.').

omega_variable(
    peripheral_economy_agency_vs_extraction,
    'Did peripheral economies passively absorb the transition''s extraction (losing external discipline, facing new volatility), or did they actively leverage the collapse of creditor discipline to gain monetary autonomy they had been denied?',
    'Policy analysis: compare monetary policy choices in peripheral economies before (1968) and after (1975, 1980) the transition. If they immediately deployed higher inflation/monetary expansion post-transition, they actively captured autonomy. If they remained tightly constrained (capital controls, low inflation), they were still extracting targets.',
    'If agency-capturing is confirmed, the transition is less universally extractive for periphery and more redistributional (they traded external discipline for internal inflation flexibility). If passive extraction is confirmed, periphery remains target seat throughout. The measured resistance (0.67) is high partly because this remains contested—some peripheral policymakers saw the transition as liberating, others as destabilizing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peripheral_economy_agency_vs_extraction, empirical, 'Whether peripheral economies gained or lost from losing the external monetary anchor.').

omega_variable(
    kernel_multiplicity_vs_reading_multiplicity,
    'Does the composite_overdetermination_reading decompose the gold_fiat_transition_mechanism kernel (one event, multiple readings) or does it claim that the kernel itself was never singular—that the historical record shows multiple distinct transitions (technology, labor, institutional) that get conflated under one label?',
    'Close reading of contemporary sources (central bank documents, labor union statements, IMF internal memos, telco industry analysis) 1968–1975: do all parties refer to ''the monetary transition'' as one event, or do they separately identify and contest distinct transitions? If they unify under one label despite discussing distinct mechanisms, the kernel is singular and this is a reading. If sources identify separate transitions, the kernel is composite and should be decomposed to separate constraint stories.',
    'This omega is meta-structural: it determines whether the constraint story is well-framed as a kernel reading or should instead be rewritten as a constraint-family (multiple interacting constraints linked by network edges). If the latter, the reading_relations and axioms in cs_structure become incorrect because they assume a single kernel. The story would need refactoring before compilation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_multiplicity_vs_reading_multiplicity, conceptual, 'Whether this is a reading of a single kernel or evidence that the kernel itself is multiple.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__composite_overdetermination_reading, 1950, 1985).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1950, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement_basis(gold_tr_t1950, observed).
narrative_ontology:measurement(gold_tr_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1960, 0.16).
narrative_ontology:measurement_basis(gold_tr_t1960, observed).
narrative_ontology:measurement(gold_tr_t1968, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1968, 0.24).
narrative_ontology:measurement_basis(gold_tr_t1968, observed).
narrative_ontology:measurement(gold_tr_t1971, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1971, 0.31).
narrative_ontology:measurement_basis(gold_tr_t1971, observed).
narrative_ontology:measurement(gold_tr_t1977, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1977, 0.32).
narrative_ontology:measurement_basis(gold_tr_t1977, observed).
narrative_ontology:measurement(gold_tr_t1985, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1985, 0.28).
narrative_ontology:measurement_basis(gold_tr_t1985, observed).

% Extraction over time
narrative_ontology:measurement(gold_be_t1950, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement_basis(gold_be_t1950, observed).
narrative_ontology:measurement(gold_be_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1960, 0.42).
narrative_ontology:measurement_basis(gold_be_t1960, observed).
narrative_ontology:measurement(gold_be_t1968, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1968, 0.48).
narrative_ontology:measurement_basis(gold_be_t1968, observed).
narrative_ontology:measurement(gold_be_t1971, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1971, 0.62).
narrative_ontology:measurement_basis(gold_be_t1971, observed).
narrative_ontology:measurement(gold_be_t1977, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1977, 0.65).
narrative_ontology:measurement_basis(gold_be_t1977, observed).
narrative_ontology:measurement(gold_be_t1985, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement_basis(gold_be_t1985, observed).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1950, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1950, 0.28).
narrative_ontology:measurement_basis(gold_su_t1950, observed).
narrative_ontology:measurement(gold_su_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1960, 0.35).
narrative_ontology:measurement_basis(gold_su_t1960, observed).
narrative_ontology:measurement(gold_su_t1968, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1968, 0.42).
narrative_ontology:measurement_basis(gold_su_t1968, observed).
narrative_ontology:measurement(gold_su_t1971, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1971, 0.48).
narrative_ontology:measurement_basis(gold_su_t1971, observed).
narrative_ontology:measurement(gold_su_t1977, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1977, 0.45).
narrative_ontology:measurement_basis(gold_su_t1977, observed).
narrative_ontology:measurement(gold_su_t1985, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1985, 0.42).
narrative_ontology:measurement_basis(gold_su_t1985, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__composite_overdetermination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.12).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, creditor_discipline_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, labor_bargaining_power_shift_1960s_1970s).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, telecommunications_infrastructure_maturation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the gold_fiat_transition_mechanism kernel. The automatic_constraint_reading frames the transition as elimination of a physical constraint (gold reserves limit money supply), replacing it with discretionary institutional authority. The creditor_discipline_reading frames it as elimination of a political constraint (creditor redemption threats discipline debtors). This reading (composite_overdetermination) claims both framings misattribute causality to a non-existent unified transition: the historical transition was overdetermined by multiple independent structural changes (technology, labor, fiscal, institutional) that happened to align 1968–1975. If correct, the transition would have occurred via any one of these factors alone; no single cause was necessary. The three readings are linked by network edges because they all seek to explain the same historical event (the Bretton Woods collapse and float adoption), but they generate different constraint types and beneficiary structures. The automatic and creditor readings treat the transition as singular and necessary; this reading treats it as composite and contingent on multiple independent forces aligning.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gold_fiat_transition_mechanism__composite_overdetermination_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
