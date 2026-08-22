% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__overdetermined_composite_reading, []).

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
 *   constraint_id: monetary_anchor_principle__overdetermined_composite_reading
 *   human_readable: Overdetermined Composite Pressure on Gold Standard Anchor (1960s-1971)
 *   domain: monetary/economic/political
 *
 * SUMMARY:
 *   The Bretton Woods gold standard collapsed in 1971 under pressure from at
 *   least four structural causes: the Triffin dilemma (a reserve-currency
 *   issuer must run deficits, exhausting gold reserves), Vietnam War fiscal
 *   deficits (policy choice colliding with monetary constraint), Keynesian
 *   policy consensus (demand-management doctrine justified expansionist
 *   policy), and private capital mobility (arbitrage and flight accelerating
 *   the regime's demise). This reading emphasizes that NO SINGLE CAUSE WAS
 *   SUFFICIENT—all four pressures were necessary to overdetermine the
 *   collapse by the late 1960s. The constraint is therefore a tangled_rope:
 *   genuine coordination (the gold standard did solve an international
 *   monetary problem), but coupled with extraction (the US extracted fiscal
 *   capacity from creditor nations and gold-reserve holders). The regime's
 *   collapse removed the extraction mechanism but left the underlying
 *   structural pressures unresolved, shifting rather than eliminating the
 *   tension.
 *
 * KEY AGENTS:
 *   - US Federal Government: sets and administers the monetary regime; runs deficits justified by Keynesian theory; beneficiary of the extracted fiscal capacity
 *   - Keynesian Policymakers: institutional beneficiaries of the removal of monetary discipline; their consensus drove the policy choices that activated the Triffin dilemma
 *   - Creditor Nations: powerful but constrained; bore the real cost of US deficit spending through reserve depreciation and inflation
 *   - Private Capital Markets: organized actors with arbitrage opportunities; their mobility accelerated the gold standard's demise
 *   - Gold-Reserve Holders: trapped victims; bore opportunity cost of non-yielding asset under inflationary policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, 0.78).
domain_priors:suppression_score(monetary_anchor_principle__overdetermined_composite_reading, 0.71).
domain_priors:theater_ratio(monetary_anchor_principle__overdetermined_composite_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(monetary_anchor_principle__overdetermined_composite_reading, "Overdetermined Composite Pressure on Gold Standard Anchor (1960s-1971)").
narrative_ontology:topic_domain(monetary_anchor_principle__overdetermined_composite_reading, "monetary/economic/political").

domain_priors:requires_active_enforcement(monetary_anchor_principle__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__overdetermined_composite_reading, '5c247578-de9c-4f5e-8cdb-8eaf0b51f47c').
narrative_ontology:cs_kernel_codification('5c247578-de9c-4f5e-8cdb-8eaf0b51f47c', fixed_text).
narrative_ontology:cs_authority_grounding('5c247578-de9c-4f5e-8cdb-8eaf0b51f47c', extraction).
narrative_ontology:cs_interpretation_layer_present('5c247578-de9c-4f5e-8cdb-8eaf0b51f47c').
narrative_ontology:cs_reading_relation('5c247578-de9c-4f5e-8cdb-8eaf0b51f47c', monetary_anchor_principle__punctuated_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('5c247578-de9c-4f5e-8cdb-8eaf0b51f47c', monetary_anchor_principle__triffin_inevitability_reading, influences).
narrative_ontology:cs_axiom('5c247578-de9c-4f5e-8cdb-8eaf0b51f47c', foundational, structural_overdetermination_thesis).
narrative_ontology:cs_axiom_status(structural_overdetermination_thesis, holdable).
narrative_ontology:cs_axiom_grounding('5c247578-de9c-4f5e-8cdb-8eaf0b51f47c', structural_overdetermination_thesis, empirically_contingent).
narrative_ontology:cs_axiom('5c247578-de9c-4f5e-8cdb-8eaf0b51f47c', secondary, fiscal_capacity_extraction_mechanism).
narrative_ontology:cs_axiom_status(fiscal_capacity_extraction_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('5c247578-de9c-4f5e-8cdb-8eaf0b51f47c', fiscal_capacity_extraction_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('5c247578-de9c-4f5e-8cdb-8eaf0b51f47c', gold_backed_dollar_anchor).
narrative_ontology:cs_drift_state('5c247578-de9c-4f5e-8cdb-8eaf0b51f47c', late_1960s_crisis, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('5c247578-de9c-4f5e-8cdb-8eaf0b51f47c', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, us_fiscal_capacity).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policymakers).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, monetary_discipline_constraint).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, creditor_nations).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, gold_reserve_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, private_capital_markets).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, international_commerce_participants).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, private_capital_markets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the Bretton Woods currency anchor (gold-backed dollar) and maintained the monetary regime. Simultaneously ran persistent fiscal deficits (Vietnam War, Great Society) justified by Keynesian demand-management doctrine. Maintained discretion over fiscal policy by suppressing the inflation signal that gold discipline would have produced. The agenda-setting power lay in choosing which pressures to acknowledge and when to exit the system.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, us_federal_government, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefited from the ability to run counter-cyclical fiscal stimulus without the monetary discipline constraint. The gold standard would have forced contraction or inflation acknowledgment; its eventual collapse removed the constraint. Their policy consensus was vindicated by the regime change, even though the transition itself created the conditions they used it to justify.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policymakers, beneficiary,
    institutional, biographical, mobile, national).

% Accumulated dollars under the Bretton Woods system and held them as reserves. When US deficits mounted, the value of those reserves deteriorated through inflation that the gold standard was supposed to prevent. They bore the real cost of US fiscal expansion. Their option to exit (demand gold redemption) was constrained by the systemic role of the dollar and by collective-action problems among creditor nations.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, creditor_nations, payer,
    powerful, generational, constrained, global).

% Profited from capital mobility and interest-rate arbitrage opportunities created by the gap between fixed parities and diverging monetary policies. They bore pressure from the constraint (capital controls, exchange regulations) but gained from the regime's eventual collapse and shift to floating rates. Their behavior (capital flight) accelerated the gold standard's demise.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, private_capital_markets, beneficiary,
    organized, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__overdetermined_composite_reading, private_capital_markets, payer).

% Central banks, treasuries, and private hoarders holding physical gold under the expectation of monetary backing. They bore the opportunity cost of holding a non-yielding asset while monetary policy delivered inflation. Their inability to exit without signaling regime collapse (and thus accelerating it) kept them trapped.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, gold_reserve_holders, payer,
    moderate, biographical, trapped, global).

% Were excluded from Bretton Woods governance and design but depended on the system for trade and capital flows. They bore contagion effects of US policy (inflation, currency instability, capital flight) without input into the regime's maintenance or transition.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, developing_nations, excluded,
    moderate, generational, constrained, global).

% Benefited from the Bretton Woods coordination of exchange rates and predictable international pricing. The fixed-parity system reduced transaction costs and currency risk for international trade.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, international_commerce_participants, beneficiary,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monetary_anchor_principle__overdetermined_composite_reading, us_federal_government).
narrative_ontology:fixing_cost_class(monetary_anchor_principle__overdetermined_composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Bretton Woods gold-standard system solved a coordination problem: post-WWII capital flows needed a common anchor for exchange rates, trade pricing, and monetary confidence. The dollar-as-anchor coordinated international commerce and capital allocation around a fixed numeraire.
% TRANSFER_FUNCTION: Transferred monetary discipline from the US government to creditor nations and gold-reserve holders: the US extracted fiscal capacity by running deficits that other nations' reserve accumulation financed. When the anchor failed, the transfer reversed: US fiscal expansion was no longer backed, and inflation redistributed wealth from reserve-holders and creditors to US debtors.
% ABSENT_VOICES: Developing nations and non-aligned countries were excluded from the original Bretton Woods governance (only the US, UK, and allied nations designed it) and had no effective voice in deciding when or how it would collapse. They bore contagion effects of US policy without input. Private capital markets also lacked formal voice but exercised pressure through arbitrage and flight.
% DISAPPEARANCE_RATIONALE: The constraint that anchored international money depended on gold redemption and US fiscal restraint. If it had not collapsed, either the Triffin dilemma would have forced it later under different circumstances, or the Vietnam War deficits and Keynesian stimulus would have been impossible—the entire post-1971 trajectory of US foreign policy, monetary expansion, and capital mobility would not have occurred. The world would have rearranged around a different monetary order (either reformed gold standard, capital controls, or sectoral monetary anchors).
% FOUNDING_PROBLEM: The original Bretton Woods problem (1944): the world needed a stable international monetary anchor after the gold standard's collapse in the 1930s had produced competitive devaluations and depression. The solution: a gold-backed dollar, with the US as the guarantor, fixing exchange rates and enabling trade.
% FOUNDING_PROBLEM_CORROBORATION: By 1968, the founding problem (lack of a monetary anchor) had been solved so effectively that the solution itself became unsustainable. Independent economic historians (Eichengreen, Steil) and contemporary observers outside the benefiting parties (French officials, Swiss bankers, academic monetary economists) attested that the original problem—monetary chaos and currency instability—no longer existed by the late 1960s, and the gold standard persisted as a vestigial constraint, not as a solution to an active problem.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__overdetermined_composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__overdetermined_composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monetary_anchor_principle__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__overdetermined_composite_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__overdetermined_composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(monetary_anchor_principle__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the constraint's persistence enabled US fiscal expansion at the cost of others' monetary discipline. Suppression is moderate-high (0.71) because the regime's maintenance required active suppression of capital flight, exchange controls, and coordination among creditor nations—absent enforcement, alternatives (gold redemption, currency flight) would have surfaced earlier. Theater ratio rises from 0.18 to 0.42 because late in the interval (1968–1971), the regime's nominal function (anchoring international commerce) was already solved; most enforcement activity defended the US fiscal capacity to run deficits, not the original coordination problem. The measurement series show extractiveness rising monotonically as deficits accumulated and the Triffin dilemma tightened (1960–1971 core period, with interval t=0 at 1960, t=27 at 1987 for comparative measurement). Suppression rises more slowly, plateauing as capital controls and coordination reached their limits. Theater rises then flattens, indicating the constraint shifted from genuine coordination to performative maintenance of a regime whose original function was already achieved.
 *
 * PERSPECTIVAL GAP:
 *   The US Federal Government and Keynesian policymakers perceive the constraint as a temporary inconvenience (the Triffin dilemma was recognized as a technical problem to be managed or exited). Creditor nations and reserve-holders perceive it as an imposed asymmetry (the US extracts from others while claiming coordination necessity). Capital markets perceive it as an arbitrage opportunity (the gap between official parities and economic reality). The engine computes different type classifications for each seat from the structural data: beneficiary seats see coordination and low extraction; victim seats see extraction and asymmetric enforcement; arbitrage-capable seats see a rent opportunity. This divergence is the measurement the framework exists to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   US fiscal capacity (beneficiary) derives directionality near 0.0 (full beneficiary: the constraint enabled them to run deficits they otherwise could not). Monetary discipline (victim, a non-agent) derives from the removal of constraints on inflation, so its 'directionality' is computational (represents the abstract good that was extracted). Creditor nations (powerful victims) derive d around 0.85–0.95 (full targets: they bore the cost of others' fiscal expansion with minimal exit). Private capital markets (organized, arbitrage-capable) derive d around 0.4–0.6 (near symmetric: genuine coordination benefit from exchange stability, but also extraction opportunity from capital controls). Gold-reserve holders (moderate, trapped) derive d around 0.9 (near-target: they bore the opportunity cost with no realistic exit). The derivation chain runs: beneficiary/victim declarations → power atoms → exit options → spatial scope → directionality. No overrides are required; the structural data produce the correct d values.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is classified as tangled_rope, which requires beneficiaries (coordination function), victims (asymmetric extraction), and active enforcement. The story delivers all three: the Bretton Woods system genuinely solved post-WWII international monetary coordination (beneficiary: international commerce, US fiscal capacity); it extracted fiscal capacity from creditors and reserve-holders (victims); its persistence required active suppression of capital flight, exchange controls, and coordination among creditor central banks (enforcement). The claim/metric gap is intentional and diagnostic: CLAIMED as tangled_rope (the structural reading this author endorses), MEASURED as high extractiveness (0.78) and high suppression (0.71), which are consistent with tangled_rope classification. The engine's per-seat computation should show: US Federal Government computes as rope-beneficiary (genuine coordination + low extraction from their seat); creditor nations compute as snare-victims (high extraction from their seat); capital markets compute as rope-arbitrageurs (coordination + arbitrage opportunity). This divergence is the test case for inter-institutional seat divergence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    overdetermination_vs_inevitability,
    'Is the gold standard''s collapse in 1971 overdetermined (multiple independent sufficient causes) or simply inevitable from a single dominant cause (Triffin dilemma)?',
    'Counterfactual analysis: if any ONE cause had been removed (e.g., no Vietnam War, or Keynesian consensus never adopted), would the collapse still have occurred at roughly the same time? If yes to multiple counterfactuals, overdetermination is confirmed; if only the Triffin dilemma remains sufficient, the reading collapses toward triffin_inevitability.',
    'This determines whether the constraint is a tangled_rope (multiple interlocking causes, each necessary) or a snare (one dominant extractive mechanism with narrative cover). Type assignment depends on whether addressing one pressure would have stabilized the system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overdetermination_vs_inevitability, empirical, 'Whether structural collapse required all four pressures or only one was sufficient.').

omega_variable(
    reading_fork_punctuated_vs_composite,
    'Is this reading (overdetermined composite) a genuine structural account, or is it post-hoc narrative covering what was actually a discrete institutional choice on August 15, 1971?',
    'Documentary evidence: did policymakers cite overdetermination as their rationale for the exit, or did they frame it as a discrete choice? Did elite discourse (FOMC minutes, Treasury analysis, Fed communications) foreground structural pressure or emphasize policy discretion?',
    'If policymakers acted as though they faced a composite constraint (high salience of all four pressures in decision-making), the overdetermined reading is structurally accurate. If they acted as though facing a discrete choice point (treating August 15 as a chosen swap), the punctuated_swap_reading dominates and this reading becomes a retroactive rationalization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_fork_punctuated_vs_composite, conceptual, 'Whether the composite framing reflects actual decision-making salience or post-hoc interpretation.').

omega_variable(
    beneficiary_structure_ambiguity,
    'Did US fiscal capacity truly benefit from the constraint, or did the Keynesian consensus and Vietnam War deficits cause the crisis that eventually harmed US interests?',
    'Accounting of US gains from the regime (seigniorage, imported goods at low prices, capital inflows financing deficits) vs. losses from the collapse (inflation, currency depreciation, reserve status erosion). Net accounting clarifies whether fiscal capacity was the true beneficiary or a mistaken reading of short-term gains.',
    'If US fiscal capacity genuinely benefited and the constraint extracted from creditors, this reading holds. If the constraint and its collapse both harmed US interests (structural error, not extraction), the reading becomes mislabeled and should be reclassified or merged with a causally clearer narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_ambiguity, empirical, 'Whether US beneficiary status was structural or retrospectively misattributed.').

omega_variable(
    kernel_reading_contest_resolution,
    'Which sibling reading—overdetermined_composite (this reading), punctuated_swap, or triffin_inevitability—correctly describes the structural causation of the 1971 transition?',
    'Each reading is one constraint story instantiating a different causal account of the same kernel (the monetary anchor principle under contest). The three readings coexist as live interpretations held by different scholarly and policy communities. No single resolution mechanism will eliminate all three; instead, the engine computes per-seat type and coupling metrics under each reading, producing a divergence profile that shows which reading best fits which empirical/normative data.',
    'Per-reading classification divergence reveals which causal frame best predicts policy outcomes, institutional structure, and observed extractiveness. This omega documents that the contest itself is irreducible via single-mechanism reasoning—all three readings remain structurally live.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_resolution, conceptual, 'Which causal reading of the monetary anchor transition is structurally correct—overdetermination, institutional choice, or Triffin inevitability?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__overdetermined_composite_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t0, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(mone_tr_t3, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 3, 0.22).
narrative_ontology:measurement(mone_tr_t6, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement(mone_tr_t9, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 9, 0.31).
narrative_ontology:measurement(mone_tr_t12, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(mone_tr_t15, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(mone_tr_t18, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 18, 0.4).
narrative_ontology:measurement(mone_tr_t21, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 21, 0.41).
narrative_ontology:measurement(mone_tr_t24, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement(mone_tr_t27, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 27, 0.42).

% Extraction over time
narrative_ontology:measurement(mone_be_t0, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mone_be_t3, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(mone_be_t6, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(mone_be_t9, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 9, 0.62).
narrative_ontology:measurement(mone_be_t12, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(mone_be_t15, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 15, 0.73).
narrative_ontology:measurement(mone_be_t18, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 18, 0.75).
narrative_ontology:measurement(mone_be_t21, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 21, 0.76).
narrative_ontology:measurement(mone_be_t24, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 24, 0.77).
narrative_ontology:measurement(mone_be_t27, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 27, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t0, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(mone_su_t3, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 3, 0.57).
narrative_ontology:measurement(mone_su_t6, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 6, 0.61).
narrative_ontology:measurement(mone_su_t9, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 9, 0.63).
narrative_ontology:measurement(mone_su_t12, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(mone_su_t15, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(mone_su_t18, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 18, 0.7).
narrative_ontology:measurement(mone_su_t21, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 21, 0.71).
narrative_ontology:measurement(mone_su_t24, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(mone_su_t27, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 27, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__overdetermined_composite_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(monetary_anchor_principle__overdetermined_composite_reading, 0.25).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle__punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle__triffin_inevitability_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the monetary_anchor_principle kernel family. The three readings (overdetermined_composite, punctuated_swap, triffin_inevitability) decompose the contested causation of the 1971 gold standard collapse into three structurally distinct constraint stories. The overdetermined_composite reading (this file) emphasizes multiple necessary causes and tangled_rope classification. The triffin_inevitability reading emphasizes a single dominant structural cause and may compute as mountain or snare depending on empirical fit. The punctuated_swap reading emphasizes institutional discretion and may compute as rope or scaffold. Per-seat type divergence across readings reveals which causal frame best captures each seat's actual structural relationship to the regime transition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monetary_anchor_principle__overdetermined_composite_reading, powerful, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
