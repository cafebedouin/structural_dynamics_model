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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Monetary Anchor Principle Under Gold Standard (Overdetermined Composite Reading)
 *   domain: economic/political/international
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'monetary_anchor_principle': specifically, the overdetermined-composite
 *   reading. The gold standard did not collapse because of a single
 *   institutional decision on August 15, 1971 (punctuated-swap reading) nor
 *   because of the Triffin dilemma alone (triffin-inevitability reading), but
 *   because of the simultaneous operation of at least four structural
 *   pressures that made collapse inevitable by the late 1960s. These
 *   pressures—the logical contradiction between reserve currency issuance and
 *   gold backing (Triffin structure), U.S. fiscal deficits from Vietnam War
 *   and Great Society spending, the intellectual consensus favoring Keynesian
 *   demand management over monetary discipline, and the technological fact of
 *   rapid international capital mobility—operated together to exhaust the
 *   system's tolerance. No single pressure was reversible without addressing
 *   the others; each reinforced the others. This reading treats the collapse
 *   as over-determined: multiple sufficient causes, any one of which would
 *   have been enough, all present simultaneously. The constraint captures the
 *   arrangement as it operated under this reading: a tangled coordination
 *   mechanism that benefited U.S. fiscal autonomy while extracting the cost
 *   of monetary discipline and reserve adequacy from other nations and the
 *   discipline regime itself. The beneficiary is state fiscal capacity; the
 *   victims are monetary discipline and the stability commitments of allied
 *   nations.
 *
 * KEY AGENTS:
 *   - State fiscal capacity (U.S., as reserve currency issuer): benefits from monetary expansion and deficit spending without immediate external penalty
 *   - Monetary discipline regime (abstract): loses coherence as inflation accumulates and real interest rates fall
 *   - Fixed exchange rate adherents (trading partners, allied central banks): trapped between defending pegs with depreciating dollars or abandoning the system
 *   - Gold reserve holders (France, Belgium, other nations with gold hoards): lose real value as inflation erodes purchasing power; can demand redemption but trigger system collapse
 *   - Triffin dilemma structure (analytical): the logical contradiction that makes the system inherently unstable
 *   - Capital mobility flows (analytical): the decentralized mechanism that accelerates exit when weakness is perceived
 *   - Vietnam War deficits (policy choice): the specific fiscal flow that collides with the monetary anchor
 *   - Keynesian policy consensus (intellectual/institutional): the doctrine that legitimizes fiscal expansion and resists monetary constraint
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
narrative_ontology:human_readable(monetary_anchor_principle__overdetermined_composite_reading, "Monetary Anchor Principle Under Gold Standard (Overdetermined Composite Reading)").
narrative_ontology:topic_domain(monetary_anchor_principle__overdetermined_composite_reading, "economic/political/international").

domain_priors:requires_active_enforcement(monetary_anchor_principle__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__overdetermined_composite_reading, '9b322b7a-a532-420b-ac05-25948b350338').
narrative_ontology:cs_kernel_codification('9b322b7a-a532-420b-ac05-25948b350338', formalized).
narrative_ontology:cs_authority_grounding('9b322b7a-a532-420b-ac05-25948b350338', extraction).
narrative_ontology:cs_interpretation_layer_present('9b322b7a-a532-420b-ac05-25948b350338').
narrative_ontology:cs_reading_relation('9b322b7a-a532-420b-ac05-25948b350338', monetary_anchor_principle__punctuated_swap_reading, influences).
narrative_ontology:cs_reading_relation('9b322b7a-a532-420b-ac05-25948b350338', monetary_anchor_principle__triffin_inevitability_reading, coexists_with).
narrative_ontology:cs_axiom('9b322b7a-a532-420b-ac05-25948b350338', foundational, structural_overdetermination_principle).
narrative_ontology:cs_axiom_status(structural_overdetermination_principle, holdable).
narrative_ontology:cs_axiom_grounding('9b322b7a-a532-420b-ac05-25948b350338', structural_overdetermination_principle, empirically_contingent).
narrative_ontology:cs_axiom('9b322b7a-a532-420b-ac05-25948b350338', secondary, fiscal_autonomy_extraction_legitimized_by_demand_management).
narrative_ontology:cs_axiom_status(fiscal_autonomy_extraction_legitimized_by_demand_management, overridden).
narrative_ontology:cs_axiom_grounding('9b322b7a-a532-420b-ac05-25948b350338', fiscal_autonomy_extraction_legitimized_by_demand_management, instrumental).
narrative_ontology:cs_reference_frame('9b322b7a-a532-420b-ac05-25948b350338', gold_standard_parity_commitment).
narrative_ontology:cs_drift_state('9b322b7a-a532-420b-ac05-25948b350338', late_1960s_terminal_phase, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('9b322b7a-a532-420b-ac05-25948b350338', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, state_fiscal_capacity).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, monetary_discipline_regime).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, fixed_exchange_rate_adherents).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, gold_reserve_holding_nations).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__overdetermined_composite_reading, keynesian_demand_management_necessity).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__overdetermined_composite_reading, capital_mobility_structural_reality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The U.S. state, as issuer of the reserve currency, benefits from the gold standard anchor by obtaining fiscal space for Keynesian demand management (Vietnam War deficits, Great Society spending) without immediate external constraint. The arrangement allows deficit spending and monetary expansion that would otherwise trigger gold loss or currency devaluation. The state administers monetary policy within this framework and has incentive to maintain the fiction of the peg while pursuing expansionary fiscal goals.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, state_fiscal_capacity, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__overdetermined_composite_reading, state_fiscal_capacity, agenda_setter).

% The abstract commitment to price stability and hard-currency constraint on government spending. This regime bears the cost of the gold standard's arbitrage: when the state pursues fiscal expansion, the discipline mechanism is eroded—inflation emerges, real interest rates fall, and the restraining force of the monetary anchor weakens. The regime cannot exit; it can only degrade.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, monetary_discipline_regime, payer,
    powerful, civilizational, trapped, global).

% Trading partners and allied nations committed to Bretton Woods fixed exchange rates and gold-convertible reserves: Britain, Germany, Japan, France. They maintain dollar pegs in their own monetary policy and accept dollar reserves as quasi-gold backing for their currencies. As U.S. deficits accelerate and the dollar weakens, these nations absorb the cost: either they must defend their own pegs (accumulating depreciating dollars) or abandon the system and re-peg, losing the stability anchor. By late 1960s they are trapped between two losses.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, fixed_exchange_rate_adherents, payer,
    powerful, generational, constrained, global).

% Nations holding gold reserves under the Bretton Woods system see the real value of those reserves decline as inflation erodes purchasing power and the nominal dollar (to which they are pegged) depreciates against gold. They have contractual right to exchange dollars for gold at $35/oz, but exercising this option (as France began to do in the mid-1960s) accelerates the system's collapse and invites retaliation. Their exit—demanding physical gold redemption—is the very mechanism that triggers the end of the system.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, gold_reserve_holding_nations, payer,
    organized, biographical, constrained, global).

% The logical-structural fact that a reserve currency issuer under gold standard must run deficits to supply global liquidity, which eventually exhausts reserves and forces abandonment. This is not an agent but an analytical structure—the contradiction between the roles of reserve currency supplier (requiring deficit) and gold-backed anchor (requiring surplus). It would object to being treated as a contingent policy choice rather than structural necessity.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, triffin_dilemma_structure, excluded,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(monetary_anchor_principle__overdetermined_composite_reading, triffin_dilemma_structure).

% The technological and institutional fact that capital can move rapidly across borders once it perceives a currency anchor weakening. Dollar outflows accelerated in the late 1960s as speculators anticipated devaluation, placing direct pressure on gold reserves. This mechanism is not authored by any agent but emerges from decentralized actor behavior in response to perceived weakness.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, capital_mobility_flows, excluded,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(monetary_anchor_principle__overdetermined_composite_reading, capital_mobility_flows).

% The specific fiscal flow: massive Pentagon spending on the Vietnam War (1965–1973) pushed U.S. fiscal deficits to levels incompatible with gold standard maintenance. This is a policy choice (invasion/escalation) that collides with the monetary anchor but is not authored by the monetary system itself.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, vietnam_war_deficits, excluded,
    analytical, biographical, analytical, national).
narrative_ontology:stakeholder_non_agent(monetary_anchor_principle__overdetermined_composite_reading, vietnam_war_deficits).

% The intellectual and institutional commitment of 1960s policymakers to full-employment demand management through fiscal stimulus and monetary accommodation. This consensus (vindicated by this reading's axiom) created political economy pressure to defend fiscal space against monetary constraint. The consensus is neither authored nor defended by any single institutional actor but emerges from the layer of economic doctrine adopted across central banks and treasuries.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policy_consensus, excluded,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policy_consensus).

% The International Monetary Fund and the institutional apparatus created in 1944 to manage fixed exchange rates and gold convertibility. By the late 1960s, the IMF recognizes it is administering a system under terminal strain and coordinates (ineffectually) among members to prop up the arrangement. The authority structure cannot change the fundamental tension it was designed to manage.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, bretton_woods_authority_structure, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monetary_anchor_principle__overdetermined_composite_reading, state_fiscal_capacity).
narrative_ontology:fixing_cost_class(monetary_anchor_principle__overdetermined_composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international exchange rates and liquidity provision under a single standard (gold parity of the U.S. dollar): trading partners know their currency value, central banks hold reserves with known backing, short-term volatility is suppressed. The gold anchor is a real coordination mechanism for cross-border commerce and capital flows.
% TRANSFER_FUNCTION: The arrangement transfers monetary policy autonomy from peripheral nations to the U.S. (as reserve currency issuer) and transfers the cost of liquidity supply (running deficits to satisfy global reserve demand) into the system as a whole: the issuance of dollars not backed by gold accumulates as a latent claim against the system's finite gold stock. When the claim is exercised (redemption demand), it collapses.
% ABSENT_VOICES: Economists and policy advisors who argued from the mid-1960s onward (Keynes, Triffin, Roosa, Mundell) that the system was inherently contradictory and unsustainable are present in the record but powerless: they cannot force institutional change. Citizens in nations experiencing imported inflation from dollar expansion, and who bear the hidden cost of the system, are absent from the bargaining table entirely. Future generations bearing the inflation that follows the system's collapse are not yet present to object.
% DISAPPEARANCE_RATIONALE: If the gold standard had held (or had never been attempted), the world monetary order would have been radically different: either the U.S. would have been forced to accept fiscal constraint (no Vietnam War spending boom, no Great Society, different defense posture), or the system would have broken apart decades earlier through different mechanisms. The arrangement held sway over the entire structure of fiscal possibility for a decade and a half; its collapse forced a fundamental reorganization into floating currencies, dollar hegemony without gold backing, and a different regime of capital controls and monetary coordination.
% FOUNDING_PROBLEM: Reconstruction of post-WWII international commerce required a stable numeraire and a mechanism for international settlements. Gold standard was the inherited solution but faced immediate contradiction: the U.S. was both the largest economy and the only nation with sufficient gold reserves to serve as reserve currency issuer, yet the role of issuer required running deficits (to supply liquidity) which undermined the gold anchor itself.
% FOUNDING_PROBLEM_CORROBORATION: By 1968 (the Gold Pool collapse) and certainly by 1971 (Nixon Shock), the founding problem of post-war commercial stability was recognized by the IMF, central banks, and economic historians as having been solved—but not by the gold standard mechanism. Instead, solutions emerged from institutional adaptation (forward markets, swap lines, SDR creation) and acceptance of floating rates. The problem the system was designed for no longer required the system's persistence. Triffin (1960) and subsequent academic analysis by economists outside benefiting institutions documented that the system's own logic made it unsustainable.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__overdetermined_composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__overdetermined_composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from 0.22 (1944, the system's initiation) to 0.78 (1971, collapse) because the gap between the coordination function (exchange rate stability) and the extraction function (U.S. fiscal autonomy at the cost of others' monetary constraint) widens over time. Early in the interval the system delivers genuine coordination benefit: the peg is stable, trade flows are predictable, reconstruction is enabled. By the 1960s, the coordination function is intact but increasingly subordinated to the extraction function: the arrangement becomes primarily a mechanism for extracting seigniorage (issuing dollars with declining backing) and transferring the inflation cost to allies. Theater ratio rises from 0.08 to 0.42 because enforcement activity increasingly focuses on defending the fiction of the peg (forward markets, gold pools, capital controls, coordination meetings) rather than on the underlying coordination itself. The suppression requirement rises from 0.35 to 0.71 as the active work to keep the system functioning intensifies: by 1968, the London Gold Pool has been abandoned, and by 1971 capital controls and emergency measures are in place to suppress speculation and capital flight. Accessibility collapse declines slightly (from 0.85 structural to 0.72) because participants increasingly recognize alternatives exist; the theoretical purity of the gold anchor is replaced by visible improvisation. The coercion grid captures the leveled dynamics: at the structural level, the Triffin contradiction becomes more visible and less suppressible; at the organizational level (central banks), suppression effort peaks as they attempt coordination; at the class level (savers, fixed-income earners), the inflation cost is diffuse and mounting; at the individual level, escape is initially available (emigration, gold hoarding, capital flight) but progressively constrained.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. state's seat (beneficiary, agenda-setter): the arrangement is a coordination mechanism that happens to permit fiscal autonomy; the expansion is justified by full-employment doctrine; the system is sustainable. From the monetary discipline regime's seat (abstract, but embodied by creditors, central banks, savers): the arrangement is extraction masked as coordination; the expansion is illegitimate monetization; the system is terminal. From the peripheral nations' seat (payers, constrained): the arrangement is subordination to U.S. hegemony; they absorb inflation imported from Washington; they are trapped between losses. The engine computes these divergences from the structural data—beneficiary/victim declarations, power levels, exit options. The seat-level directionality computation should show state fiscal capacity near d≈0.0 (full beneficiary), monetary discipline and fixed-rate adherents near d≈1.0 (full targets), and the peripheral nations somewhere between 0.6 and 0.9 depending on their specific exit options. The divergence is not error; it is the signal that this reading's core claim—overdetermination by multiple structural pressures—generates asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   State fiscal capacity (d→0.0): the U.S. as reserve currency issuer collects the seigniorage (benefit from issuing currency with declining backing). It controls the rules (deficit spending, monetary expansion) and has highest exit option (can unilaterally abandon the peg, as Nixon did). Monetary discipline regime (d→1.0): the abstract commitment to hard currency and inflation control is structurally victimized by the arrangement; it cannot exit but only degrade. Fixed exchange rate adherents (d→0.8–0.9): allied nations are organized and powerful but trapped by contractual commitment and the dollar's role in their own reserve strategy. They pay in inflation import and currency instability; their exit (floating rates) is available but catastrophic (currency wars). Gold reserve holders (d→0.7–0.8): they have commodity backing for their reserves but watch it depreciate in real terms; redemption is theoretically possible but triggers system collapse, so they are trapped. Keynesian consensus (d→0.2): the intellectual coalition benefits from the arrangement's permission of demand management; it is not victimized but is complicit. The directionality derivation from these beneficiary/victim + power + exit declarations should produce a seat divergence: high d for the peripheral nations and the discipline regime, low d for the U.S., medium d for the intellectual consensus. The author declares the empirical facts (who benefits, who bears costs, what exits are available); the engine computes d from that structure and produces the per-seat classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading treats the founding problem (post-war settlement and commercial stability) as DEAD by 1968–1971. The problem was SOLVED—not by the gold standard itself but by institutional adaptation (forward markets, swap lines, SDR creation, capital controls) and by the acceptance of floating rates. Yet the arrangement persisted for decades after its problem was solved, extracting rent (seigniorage) and maintaining U.S. fiscal autonomy beyond the original justification. The divergence between founding problem status (dead) and disappearance verdict (world rearranges) is not a contradiction; it is the mandatrophy signal. The system persisted not because it solved a live coordination problem but because it served extractive interests (U.S. fiscal capacity, Keynesian consensus). By 1971 the arrangement was entirely theater: the gold pools were broken, redemption was suspended (except on paper), and capital controls were in place. The measured theater_ratio rise (0.08→0.42) captures this drift: enforcement activity increasingly defends the peg's reputation rather than its substance. The system did not collapse because it failed to solve its problem; it collapsed because its problem had been solved and the arrangement persisted solely for extraction—which eventually became unsustainable when participants recognized the fiction and organized exit (France demanding gold, speculators attacking the dollar). Mandatrophy is resolved: the arrangement's mandate (post-war stability) outlived the arrangement itself, and once the mandate was recognized as solved, the extraction-only remainder became indefensible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    triffin_dilemma_necessity_vs_contingency,
    'Is the Triffin dilemma a logically necessary structural contradiction, or is it a contingent policy choice problem that could have been resolved through alternative institutional design (e.g., Keynes''s bancor, an international reserve asset not tied to any single national currency)?',
    'Counterfactual historical analysis or examination of contemporaneous proposals (Triffin Plan, SDR, Keynes''s original proposals) to determine whether alternatives were technically feasible or politically foreclosed.',
    'If the dilemma was necessary, then the collapse was overdetermined in a deeper sense—even optimal policy could not have averted it. If contingent, then the arrangement persisted due to institutional path-dependence rather than logical necessity, and the overdetermination reading applies (multiple pressures that could have been individually addressed but were not).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(triffin_dilemma_necessity_vs_contingency, conceptual, 'Whether the Triffin contradiction was logically necessary or institutionally contingent.').

omega_variable(
    vietnam_deficit_counterfactual,
    'If the U.S. had not pursued the Vietnam War (or had funded it through explicit taxation rather than deficit spending), would the gold standard have persisted beyond the early 1970s?',
    'Examination of the monetary dynamics absent the Vietnam deficit: modeling of U.S. gold reserves, capital flows, and allied confidence under an alternative fiscal scenario.',
    'If the system would have persisted substantially longer or indefinitely without the Vietnam deficit, then overdetermination is weak—the Vietnam deficit was the binding constraint, not one among multiple. If the system would still have collapsed due to the other pressures (Triffin structure, capital mobility, Keynesian consensus), then overdetermination is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vietnam_deficit_counterfactual, empirical, 'Whether Vietnam deficits were a necessary or merely sufficient cause of collapse.').

omega_variable(
    capital_mobility_acceleration,
    'How much of the acceleration of the collapse in the late 1960s was due to the technological and institutional facts of capital mobility versus to the policy choices (Vietnam spending, monetary expansion) that motivated the exits?',
    'Analysis of capital flow timing relative to policy changes: if capital exits accelerated AFTER policy shifts were announced/executed, then capital mobility is a trigger; if they occurred BEFORE, then expectations and the underlying structural contradiction are the trigger.',
    'If capital mobility was primarily a trigger (responding to policy), then the overdetermination attribution shifts: the primary causes are policy choices + structural pressures, and capital mobility is the mechanism. If capital mobility was a direct structural fact (independent of policy choice), then overdetermination includes capital technology as a fourth fundamental pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_mobility_acceleration, empirical, 'Whether capital mobility was a primary structural pressure or a secondary trigger mechanism.').

omega_variable(
    reading_boundary_punctuated_vs_composite,
    'Does the distinction between this reading (overdetermined composite) and the punctuated-swap reading (single institutional choice on August 15, 1971) hinge on whether we treat the structural pressures as forcing a collapse (composite) or merely as making a collapse possible without determining its timing (punctuated)?',
    'Examination of historical counterfactuals: would the system have persisted if Nixon had chosen not to suspend convertibility on August 15? Or was suspension inevitable by that date? If persistence was possible but institutionally rejected, then punctuated reading wins; if persistence was impossible, then overdetermined reading wins.',
    'If the pressures made persistence impossible by 1971, then this reading (composite) is the true causal account and punctuated is a misattribution of contingency to what was actually necessary. If persistence was theoretically possible but rejected, then both readings coexist: the pressures were overdetermined at the level of causation, but the specific date and mechanism were contingent institutional choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_punctuated_vs_composite, conceptual, 'Whether overdetermination and punctuation are competing readings (forecloses) or complementary descriptions (coexists_with).').

omega_variable(
    seigniorage_distribution_asymmetry,
    'Was the seigniorage gain (the U.S. benefit from issuing reserve currency at less-than-real backing) knowingly and intentionally extracted by U.S. policymakers, or was it an unintended side effect of legitimate policy choices (Vietnam spending, Keynesian demand management)?',
    'Examination of contemporary policy documents, Federal Reserve internal discussions, and Treasury deliberations: did policymakers explicitly frame deficit spending as a strategy to extract seigniorage, or did they justify it on Keynesian/geopolitical grounds and rationalize the seigniorage afterward?',
    'If intentional extraction, then the arrangement is snare-like (pure extraction defended by false coordination narrative). If unintended side effect, then the arrangement is more correctly tangled_rope (genuine coordination that enabled extraction as a byproduct). The ambiguity affects the beneficiary characterization: is state_fiscal_capacity a knowing extractor or a lucky beneficiary of structural drift?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seigniorage_distribution_asymmetry, empirical, 'Whether the extraction was intentional policy or unintended institutional side effect.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__overdetermined_composite_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1944, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1944, 0.08).
narrative_ontology:measurement_basis(mone_tr_t1944, observed).
narrative_ontology:measurement(mone_tr_t1950, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement_basis(mone_tr_t1950, observed).
narrative_ontology:measurement(mone_tr_t1960, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1960, 0.22).
narrative_ontology:measurement_basis(mone_tr_t1960, observed).
narrative_ontology:measurement(mone_tr_t1965, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1965, 0.31).
narrative_ontology:measurement_basis(mone_tr_t1965, observed).
narrative_ontology:measurement(mone_tr_t1968, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1968, 0.38).
narrative_ontology:measurement_basis(mone_tr_t1968, observed).
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1971, 0.42).
narrative_ontology:measurement_basis(mone_tr_t1971, observed).

% Extraction over time
narrative_ontology:measurement(mone_be_t1944, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1944, 0.22).
narrative_ontology:measurement_basis(mone_be_t1944, observed).
narrative_ontology:measurement(mone_be_t1950, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1950, 0.28).
narrative_ontology:measurement_basis(mone_be_t1950, observed).
narrative_ontology:measurement(mone_be_t1960, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1960, 0.45).
narrative_ontology:measurement_basis(mone_be_t1960, observed).
narrative_ontology:measurement(mone_be_t1965, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1965, 0.62).
narrative_ontology:measurement_basis(mone_be_t1965, observed).
narrative_ontology:measurement(mone_be_t1968, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1968, 0.71).
narrative_ontology:measurement_basis(mone_be_t1968, observed).
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1971, 0.78).
narrative_ontology:measurement_basis(mone_be_t1971, observed).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t1944, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1944, 0.35).
narrative_ontology:measurement_basis(mone_su_t1944, observed).
narrative_ontology:measurement(mone_su_t1950, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1950, 0.41).
narrative_ontology:measurement_basis(mone_su_t1950, observed).
narrative_ontology:measurement(mone_su_t1960, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1960, 0.52).
narrative_ontology:measurement_basis(mone_su_t1960, observed).
narrative_ontology:measurement(mone_su_t1965, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1965, 0.63).
narrative_ontology:measurement_basis(mone_su_t1965, observed).
narrative_ontology:measurement(mone_su_t1968, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1968, 0.68).
narrative_ontology:measurement_basis(mone_su_t1968, observed).
narrative_ontology:measurement(mone_su_t1971, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1971, 0.71).
narrative_ontology:measurement_basis(mone_su_t1971, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1944, tn=1971
narrative_ontology:measurement(mone_grid_01, monetary_anchor_principle__overdetermined_composite_reading, accessibility_collapse(class), 1944, 0.68).
narrative_ontology:measurement(mone_grid_02, monetary_anchor_principle__overdetermined_composite_reading, accessibility_collapse(class), 1971, 0.62).
narrative_ontology:measurement(mone_grid_03, monetary_anchor_principle__overdetermined_composite_reading, accessibility_collapse(individual), 1944, 0.55).
narrative_ontology:measurement(mone_grid_04, monetary_anchor_principle__overdetermined_composite_reading, accessibility_collapse(individual), 1971, 0.48).
narrative_ontology:measurement(mone_grid_05, monetary_anchor_principle__overdetermined_composite_reading, accessibility_collapse(organizational), 1944, 0.72).
narrative_ontology:measurement(mone_grid_06, monetary_anchor_principle__overdetermined_composite_reading, accessibility_collapse(organizational), 1971, 0.58).
narrative_ontology:measurement(mone_grid_07, monetary_anchor_principle__overdetermined_composite_reading, accessibility_collapse(structural), 1944, 0.85).
narrative_ontology:measurement(mone_grid_08, monetary_anchor_principle__overdetermined_composite_reading, accessibility_collapse(structural), 1971, 0.72).
narrative_ontology:measurement(mone_grid_09, monetary_anchor_principle__overdetermined_composite_reading, resistance(class), 1944, 0.14).
narrative_ontology:measurement(mone_grid_10, monetary_anchor_principle__overdetermined_composite_reading, resistance(class), 1971, 0.65).
narrative_ontology:measurement(mone_grid_11, monetary_anchor_principle__overdetermined_composite_reading, resistance(individual), 1944, 0.08).
narrative_ontology:measurement(mone_grid_12, monetary_anchor_principle__overdetermined_composite_reading, resistance(individual), 1971, 0.48).
narrative_ontology:measurement(mone_grid_13, monetary_anchor_principle__overdetermined_composite_reading, resistance(organizational), 1944, 0.18).
narrative_ontology:measurement(mone_grid_14, monetary_anchor_principle__overdetermined_composite_reading, resistance(organizational), 1971, 0.58).
narrative_ontology:measurement(mone_grid_15, monetary_anchor_principle__overdetermined_composite_reading, resistance(structural), 1944, 0.12).
narrative_ontology:measurement(mone_grid_16, monetary_anchor_principle__overdetermined_composite_reading, resistance(structural), 1971, 0.62).
narrative_ontology:measurement(mone_grid_17, monetary_anchor_principle__overdetermined_composite_reading, stakes_inflation(class), 1944, 0.18).
narrative_ontology:measurement(mone_grid_18, monetary_anchor_principle__overdetermined_composite_reading, stakes_inflation(class), 1971, 0.72).
narrative_ontology:measurement(mone_grid_19, monetary_anchor_principle__overdetermined_composite_reading, stakes_inflation(individual), 1944, 0.08).
narrative_ontology:measurement(mone_grid_20, monetary_anchor_principle__overdetermined_composite_reading, stakes_inflation(individual), 1971, 0.58).
narrative_ontology:measurement(mone_grid_21, monetary_anchor_principle__overdetermined_composite_reading, stakes_inflation(organizational), 1944, 0.22).
narrative_ontology:measurement(mone_grid_22, monetary_anchor_principle__overdetermined_composite_reading, stakes_inflation(organizational), 1971, 0.75).
narrative_ontology:measurement(mone_grid_23, monetary_anchor_principle__overdetermined_composite_reading, stakes_inflation(structural), 1944, 0.15).
narrative_ontology:measurement(mone_grid_24, monetary_anchor_principle__overdetermined_composite_reading, stakes_inflation(structural), 1971, 0.68).
narrative_ontology:measurement(mone_grid_25, monetary_anchor_principle__overdetermined_composite_reading, suppression(class), 1944, 0.32).
narrative_ontology:measurement(mone_grid_26, monetary_anchor_principle__overdetermined_composite_reading, suppression(class), 1971, 0.68).
narrative_ontology:measurement(mone_grid_27, monetary_anchor_principle__overdetermined_composite_reading, suppression(individual), 1944, 0.22).
narrative_ontology:measurement(mone_grid_28, monetary_anchor_principle__overdetermined_composite_reading, suppression(individual), 1971, 0.55).
narrative_ontology:measurement(mone_grid_29, monetary_anchor_principle__overdetermined_composite_reading, suppression(organizational), 1944, 0.35).
narrative_ontology:measurement(mone_grid_30, monetary_anchor_principle__overdetermined_composite_reading, suppression(organizational), 1971, 0.72).
narrative_ontology:measurement(mone_grid_31, monetary_anchor_principle__overdetermined_composite_reading, suppression(structural), 1944, 0.28).
narrative_ontology:measurement(mone_grid_32, monetary_anchor_principle__overdetermined_composite_reading, suppression(structural), 1971, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__overdetermined_composite_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(monetary_anchor_principle__overdetermined_composite_reading, 0.18).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle__punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle__triffin_inevitability_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, bretton_woods_institutional_framework).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, reserve_currency_privilege_extraction).

% DUAL FORMULATION NOTE:
% This constraint belongs to the 'monetary_anchor_principle' constraint family decomposed from a single natural-language kernel by the ε-invariance principle. Three readings emit three distinct constraints with different epsilon values and causal structures: (1) overdetermined_composite_reading (this story) — multiple structural pressures made collapse inevitable; type tangled_rope; epsilon ~0.78. (2) punctuated_swap_reading — a single institutional choice on August 15, 1971 created the transition; type snare; epsilon ~0.85 (pure extraction after problem solved). (3) triffin_inevitability_reading — the Triffin contradiction alone made collapse inevitable; type mountain or near-mountain; epsilon variable by reading but dilemma itself structural. The three readings share the referent (the gold standard arrangement, 1944–1971) but instantiate different causal stories and deliver different ε-invariant claims. Each story carries its own beneficiary/victim structure, stake divergence, and measured type. The upstream story (triffin_inevitability) establishes the structural boundary; the composite reading (this story) adds policy and coordination pressures; the punctuated reading attributes the specific timing to institutional choice. Downstream consumers should treat these as three distinct constraint stories, not as three 'perspectives' on one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monetary_anchor_principle__overdetermined_composite_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
