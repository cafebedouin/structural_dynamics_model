% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__punctuated_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__punctuated_swap_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: monetary_anchor_principle__punctuated_swap_reading
 *   human_readable: Bretton Woods Monetary Anchor—Punctuated Swap Reading
 *   domain: economic/political/international
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'monetary_anchor_principle': the punctuated_swap_reading, which frames
 *   the August 15, 1971 Bretton Woods suspension as a discrete institutional
 *   choice by the U.S. Treasury, not an inevitable structural collapse. Under
 *   this reading, Nixon and Connally made a reversible decision to suspend
 *   gold convertibility in order to restore U.S. fiscal autonomy and escape
 *   the Triffin dilemma's constraints on domestic monetary policy. The
 *   decision extracted wealth from foreign dollar holders (who found their
 *   reserves devalued) and imposed adjustment costs on peripheral economies.
 *   This reading treats the constraint as rope (coordination with asymmetric
 *   extraction), not as an overdetermined structural inevitability. The
 *   measurement interval spans from pre-swap (t=0: gold standard still
 *   nominally in effect) through the immediate post-swap period (t=12:
 *   floating-rate regime stabilizing). Extractiveness rises steeply in the
 *   first 6 months as the dollar devaluation shock propagates, then plateaus
 *   as markets adapt to the new floating regime.
 *
 * KEY AGENTS:
 *   - us_treasury: institutional agenda-setter (power: institutional, exit: arbitrage) — makes the August 15 decision unilaterally; benefits from restored fiscal autonomy and implicit devaluation gain
 *   - foreign_dollar_holders: organized payer (power: organized, exit: constrained) — suffer implicit expropriation when convertibility is suspended; cannot exit the dollar system en masse
 *   - us_financial_markets: powerful beneficiary (power: powerful, exit: mobile) — gain from restored monetary-policy autonomy and dollar hegemony preservation
 *   - bretton_woods_signatories: organized payer (power: organized, exit: constrained) — must absorb exchange-rate adjustment and transition costs; had negotiated the fixed-rate regime
 *   - peripheral_economies: moderate payer + excluded (power: moderate, exit: constrained) — bear import-price inflation and resource shocks; excluded from decision process
 *   - international_monetary_fund: institutional observer (power: institutional, exit: analytical) — mandate becomes moot; must be reconstructed for floating-rate world
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, 0.62).
domain_priors:suppression_score(monetary_anchor_principle__punctuated_swap_reading, 0.41).
domain_priors:theater_ratio(monetary_anchor_principle__punctuated_swap_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__punctuated_swap_reading, rope).
narrative_ontology:human_readable(monetary_anchor_principle__punctuated_swap_reading, "Bretton Woods Monetary Anchor—Punctuated Swap Reading").
narrative_ontology:topic_domain(monetary_anchor_principle__punctuated_swap_reading, "economic/political/international").

domain_priors:requires_active_enforcement(monetary_anchor_principle__punctuated_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__punctuated_swap_reading, 'f55d76ab-e43b-4404-9698-b1c0d9c03219').
narrative_ontology:cs_kernel_codification('f55d76ab-e43b-4404-9698-b1c0d9c03219', fixed_text).
narrative_ontology:cs_authority_grounding('f55d76ab-e43b-4404-9698-b1c0d9c03219', lineage).
narrative_ontology:cs_interpretation_layer_present('f55d76ab-e43b-4404-9698-b1c0d9c03219').
narrative_ontology:cs_reading_relation('f55d76ab-e43b-4404-9698-b1c0d9c03219', monetary_anchor_principle__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_reading_relation('f55d76ab-e43b-4404-9698-b1c0d9c03219', monetary_anchor_principle__triffin_inevitability_reading, coexists_with).
narrative_ontology:cs_axiom('f55d76ab-e43b-4404-9698-b1c0d9c03219', foundational, monetary_regime_choice_contingent).
narrative_ontology:cs_axiom_status(monetary_regime_choice_contingent, holdable).
narrative_ontology:cs_axiom_grounding('f55d76ab-e43b-4404-9698-b1c0d9c03219', monetary_regime_choice_contingent, empirically_contingent).
narrative_ontology:cs_axiom('f55d76ab-e43b-4404-9698-b1c0d9c03219', secondary, executive_authority_over_currency_anchor).
narrative_ontology:cs_axiom_status(executive_authority_over_currency_anchor, holdable).
narrative_ontology:cs_axiom_grounding('f55d76ab-e43b-4404-9698-b1c0d9c03219', executive_authority_over_currency_anchor, conventional).
narrative_ontology:cs_reference_frame('f55d76ab-e43b-4404-9698-b1c0d9c03219', bretton_woods_gold_backed_dollar).
narrative_ontology:cs_drift_state('f55d76ab-e43b-4404-9698-b1c0d9c03219', august_1971_suspension, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('f55d76ab-e43b-4404-9698-b1c0d9c03219', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_treasury_fiscal_autonomy).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_holders).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, peripheral_economies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_financial_markets).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, bretton_woods_signatories).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__punctuated_swap_reading, institutional_choice_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the August 15, 1971 decision to suspend gold convertibility unilaterally. Treasury and the Federal Reserve Board (Burns) judge that maintaining convertibility at $35/oz is no longer compatible with U.S. domestic monetary-policy autonomy and fiscal flexibility. The decision is characterized as 'temporary' but becomes permanent. Treasury collects the implicit devaluation gain (assets denominated in gold appreciate relative to dollar holdings abroad) and recovers the freedom to expand the money supply and run deficits without depleting gold reserves.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_treasury, agenda_setter,
    institutional, generational, arbitrage, global).

% Central banks, governments, and private institutions in Europe and Japan hold dollar reserves as backup to their own currencies, under the Bretton Woods understanding that dollars are 'as good as gold' (convertible at $35/oz). When convertibility is suspended, the dollar devalues against gold immediately (and against revaluing currencies over subsequent months). Their reserve holdings lose purchasing power in real terms. They cannot exit the dollar system en masse without destabilizing international trade finance and their own monetary bases (the dollar is the vehicle currency for settlement). They are bound by structural dependence.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_holders, payer,
    organized, biographical, constrained, global).

% Nations that negotiated and ratified the Bretton Woods Articles of Agreement in 1944 had committed to fixed exchange rates against the dollar and accepted the dollar as the anchor. The unilateral U.S. suspension of convertibility tears up that commitment without consultation or renegotiation. Signatories must now choose between revaluing their currencies (Germany, Switzerland benefit), depreciating (France, Italy, UK absorb shock), or maintaining pegs (Japan, initially constrained). The regime transition imposes substantial adjustment costs.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, bretton_woods_signatories, payer,
    organized, generational, constrained, global).

% U.S. banks, investment firms, and asset managers benefit immediately from the decision. Monetary policy is no longer constrained by gold reserves; interest rates can track domestic economic conditions and Fed objectives rather than the external-balance requirement. The dollar remains the preeminent global reserve and transaction currency despite devaluation. U.S. capital markets regain the flexibility to attract international capital and finance at cheaper rates than otherwise possible under continued gold standard.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_financial_markets, beneficiary,
    powerful, biographical, mobile, global).

% Smaller developing economies that peg to the dollar or price exports in dollars suffer import-price inflation when the dollar devalues against other major currencies (Germany, Switzerland revalue). They are also excluded from the Bretton Woods governance structure and have no voice in the August 15 decision. Their terms of trade deteriorate; they accumulate dollar debt at rising real cost; their export demand contracts as the U.S. and Europe enter recession in the early 1970s. The shock is imposed without consultation.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, peripheral_economies, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__punctuated_swap_reading, peripheral_economies, excluded).

% Created in 1944 to enforce Bretton Woods discipline and manage the fixed-exchange-rate system, the IMF's mandate becomes moot when the U.S. unilaterally exits convertibility. The IMF's authority to enforce fixed-rate discipline and its enforcement mechanism (drawing rights, quotas) are rendered obsolete. The institution must be reconstructed post-hoc as a manager of floating-rate adjustment and lender to countries in balance-of-payments crisis. The decision is a structural nullification of the IMF's founding purpose.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% Holds formal fiscal authority but is not consulted on the August 15 decision; it is a fait accompli by the Executive Branch (Nixon, Connally, Burns). Congress ratifies the transition post-hoc by authorizing new IMF arrangements and accepting the floating-rate regime without substantive debate. The decision reveals the Executive's effective monopoly over monetary-regime choice, even in matters of constitutional fiscal authority.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_congress, observer,
    institutional, generational, analytical, national).

% Economists (Triffin, Mundell, Keynes's followers, Chicago School monetarists) had theorized the Triffin dilemma, capital-account pressures, and the impossibility of maintaining fixed rates under integrated capital markets. Their analyses predicted system collapse; the August 15 decision validates their theories. However, they are excluded from the actual institutional decision and have no voice in regime design. They become post-hoc interpreters of events rather than designers.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, academic_economists, excluded,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bretton Woods solves a genuine post-WWII coordination problem: the world needs a unit of account and medium for international settlement; the gold-backed dollar provides this, replacing the chaos of competitive devaluations and bilateral barter of the 1930s. The fixed-exchange-rate mechanism enables trade finance and capital flows with predictable pricing. The problem is real; the solution is coordination.
% TRANSFER_FUNCTION: Moves the implicit seigniorage gain from gold devaluation—the difference between what dollar holders paid for dollars (at $35/oz convertibility) and what their dollar holdings are worth on the open market after devaluation—from foreign dollar holders to the U.S. Treasury and U.S. financial markets. Also transfers adjustment costs (exchange-rate shock, inflation, export-demand loss) to peripheral economies and trade-dependent developed economies. The transfer is not explicit payment but implicit wealth reallocation via regime change.
% ABSENT_VOICES: Foreign central banks and governments; developing economies; international labor affected by inflation and unemployment during adjustment; populations holding dollar savings; competitors of the U.S. financial industry who lose market share when dollar hegemony is reasserted. These groups would object to the unilateral decision and demand negotiated transition, compensation, or alternative arrangements. Instead, they face a fait accompli.
% DISAPPEARANCE_RATIONALE: If the August 15, 1971 decision to suspend convertibility never happened, Bretton Woods persists in some form through the 1970s (in degraded compliance). U.S. monetary policy remains constrained by gold reserves, limiting domestic flexibility. The world economy continues under fixed rates (or eventual negotiated adjustments rather than the shock devaluation that occurred). Subsequent capital flows, oil shocks, and inflation dynamics would play out differently. The floating-rate regime that defined the 1970s–2000s would not exist; some other regime evolution (IMF reform, reserve-currency basket, gold-price increase) would emerge instead.
% FOUNDING_PROBLEM: Post-WWII international monetary coordination: nations need a stable unit of account for cross-border trade, investment, and settlement. The prewar gold standard failed due to deflationary rigidity and competitive devaluations. Bretton Woods (1944) creates a new anchor: the U.S. dollar backed by gold convertibility, with other currencies fixed against the dollar. The founding problem is real and urgent (reconstruction of Europe and Asia, resumption of world trade).
% FOUNDING_PROBLEM_CORROBORATION: U.S. policymakers in August 1971 (Connally: 'The United States is in charge here and the rest of the world will have to adjust') attest that the founding problem is now SOLVED—so thoroughly solved that the mechanism can be dismantled and replaced with floating rates. Foreign policymakers (French Finance Minister, German Bundesbank) attest that the founding problem of stable international settlement is NOT solved by the August 15 decision; it is merely replaced with floating-rate uncertainty and dollar hegemony. Economists (Triffin) attest that the founding problem was NEVER solvable under the constraint that the U.S. maintains gold convertibility at a fixed price while supplying global liquidity. The attestations diverge sharply; no external corroborator fully validates the U.S. Treasury's framing.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__punctuated_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__punctuated_swap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__punctuated_swap_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(monetary_anchor_principle__punctuated_swap_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__punctuated_swap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(monetary_anchor_principle__punctuated_swap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the implicit transfer of wealth via devaluation shock to foreign dollar holders and the U.S. Treasury's unilateral capture of the seigniorage gain. The extraction is substantial but not total because the constraint also solves a real coordination problem (the world does need a unit of account) and most parties benefit from resumed U.S. monetary flexibility. Suppression (0.41) is moderate: foreign central banks and governments resist the decision verbally and through formal IMF negotiations, but lack the power to block it once the U.S. acts unilaterally; their 'suppression' is structural (trapped in the dollar system) rather than coercive (no violence or explicit threat). Theater ratio (0.28) is low because the functional justification for the decision is real—the Triffin dilemma is a genuine structural problem—but enforcement activity consists mainly of maintaining the new regime (preventing gold-window reopening, enforcing floating-rate acceptance) rather than defending a false naturalization. The measurement trajectory shows extractiveness and suppression rising steeply in months 0–6 as the shock propagates and resistance crystallizes, then plateauing by month 12 as markets and governments adapt. Theater ratio also rises and plateaus, reflecting the shift from active decision-making to regime maintenance.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. Treasury seat, this is a necessary and reasonable institutional repair of a broken system—a rope constraint solving a real coordination problem with acceptable asymmetry. From the foreign dollar holder seat (central banks, governments), this is an enforced expropriation via unilateral regime termination—extraction masquerading as repair. From the peripheral economy seat, this is a shock imposed without voice or consent, compounded by import-price inflation and loss of export demand as the U.S. economy adjusts. The engine should compute these divergences from power and exit-option asymmetries: the Treasury's institutional power and arbitrage exit produce low extraction at that seat; the peripheral economy's moderate power and trapped exit produce high extraction at that seat. The same constraint, experienced as coordination by one agent and extraction by another, is exactly the per-seat classification divergence the system is designed to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. Treasury is the structural beneficiary: d near 0.0 (collects the devaluation gain, controls the rules, has arbitrage exit options). Foreign dollar holders are the structural targets: d near 1.0 (bear the devaluation loss, constrained exit options—cannot dump dollars without destabilizing their own economies). Bretton Woods signatories are split: developed economies (Germany, Switzerland) get revaluation benefits and absorb modest adjustment costs (d ~0.4–0.5), while peripheral economies absorb import-price inflation and export-demand shock (d ~0.7–0.8). The engine should derive this from the beneficiary/victim declarations and exit-option asymmetries: beneficiaries get low d (Treasury, U.S. financial markets), victims get high d (foreign dollar holders, peripheral economies). Overrides are not needed; the structural data is clear.
 *
 * MANDATROPHY ANALYSIS:
 *   Under the punctuated_swap reading, the constraint's mandate (provide a stable monetary anchor for international commerce) is still live at t=0, already degraded by the Triffin dilemma's logical binding. The August 15 decision does NOT kill the mandate—it transfers the mandate from Bretton Woods (fixed gold-backed dollar) to a new regime (floating rates, dollar hegemony without convertibility). The new regime still solves the coordination problem, just with a different institutional architecture. Mandatrophy does NOT apply to this constraint at the interval end; the founding problem persists, solved by a new arrangement. The constraint classified as ROPE remains rope—a coordination mechanism with asymmetric extraction embedded in its structure, not a piton whose function has atrophied. If we were examining the post-1980 trajectory, we might find theater ratio rising (stagflation era, periodic currency crises, policy theater around 'strong dollar' language) suggesting piton-ward drift, but that is beyond this interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discretionary_choice_vs_structural_inevitability,
    'Was August 15, 1971 a discrete institutional choice by the U.S. Treasury to exit the gold standard and restore fiscal autonomy, or was it a structurally inevitable collapse of the Bretton Woods system driven by the Triffin dilemma and capital-account pressures that made the constraint unsustainable by that date?',
    'Historical counterfactual analysis: what would have happened if Nixon had chosen differently? Did alternative policy packages (capital controls, gold-reserve borrowing, coordinated IMF reform) remain available, or were they foreclosed by August 1971? Archive of policymaker deliberations (declassified NSC memos, Burns-Volcker correspondence) to establish whether decision was perceived as chosen or inevitable at the time. Comparison to other nations'' monetary decisions to test whether the U.S. faced unique pressures or faced choices similar to those other central banks made.',
    'If the choice was discrete and reversible: the constraint should be classified as rope (coordination with embedded extraction, maintained by active institutional decision-making) rather than as a natural law or overdetermined process. If the choice was structurally inevitable: the constraint might be better read as a transition process (scaffold) or as the terminal outcome of structural forces (mountain-adjacent). The classification hinges on this omega.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretionary_choice_vs_structural_inevitability, conceptual, 'Whether the August 15 decision was contingent institutional choice or structurally determined outcome.').

omega_variable(
    triffin_dilemma_timing_and_causation,
    'Did the Triffin dilemma CAUSE the August 15, 1971 decision, or did it merely CREATE PRESSURE that U.S. policymakers could have resisted via alternative mechanisms (capital controls, SDR expansion, reserve-currency basket)?',
    'Formal economic modeling: solve the Triffin constraint for each year 1960–1971 and determine when (if ever) gold reserves reach the threshold where continued convertibility is mathematically impossible. Compare to actual policy choices and constraints stated by policymakers at the time. Test whether alternative policies (gold-price increase, reserve borrowing from allies, coordinated IMF reform) could have extended convertibility beyond August 1971.',
    'If the Triffin dilemma was the driving cause: the constraint is better understood as an overdetermined or inevitable process, not a discrete choice; the reading shifts toward the ''triffin_inevitability_reading''. If the Triffin dilemma created pressure but alternatives existed: the punctuated_swap_reading holds; the decision was a choice under constraints, not an inevitable response to a mathematical limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_dilemma_timing_and_causation, empirical, 'Whether the Triffin dilemma was deterministic cause or merely background pressure.').

omega_variable(
    foreign_dollar_holder_coordination_capacity,
    'Could foreign central banks and governments have coordinated a collective response to the August 15 announcement (e.g., demanding gold redemption en masse, refusing to hold dollars, forming an alternative reserve currency) that would have forced the U.S. to negotiate rather than unilaterally impose floating rates?',
    'Archival analysis of post-August 15 diplomatic communications and IMF negotiations; game-theoretic analysis of the prisoners'' dilemma foreign holders faced (each benefited individually from accepting dollars and devaluation risk rather than coordinating to reject them). Interviews with surviving central bankers and IMF officials on what coordination mechanisms were attempted or considered.',
    'If foreign holders could have coordinated: the ''suppression'' (0.41) is not structural extraction but rather a failure of collective action; the constraint is less snare-like and more rope-like (a coordination failure between equal parties). If foreign holders lacked coordination capacity: the suppression reflects structural power asymmetry and the constraint is more clearly extractive (higher snare-affinity). The classification might shift toward snare if suppression is strategic rather than structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foreign_dollar_holder_coordination_capacity, empirical, 'Whether foreign resistance was suppressed by U.S. power or by collective-action failure.').

omega_variable(
    kernel_reading_contest,
    'Which reading of the ''monetary_anchor_principle'' kernel is the correct account of what August 15, 1971 WAS: a discrete institutional choice (punctuated_swap), an overdetermined structural collapse (overdetermined_composite), or a mathematically inevitable outcome of the Triffin dilemma (triffin_inevitability)?',
    'This omega names the fundamental uncertainty that splits the three readings. No single empirical resolution fully adjudicates it—it depends on what causal framework one accepts. Resolution requires philosophical clarification of what counts as ''choice'' vs. ''inevitability'' in institutional settings where constraints are real but not mathematical. The three readings can coexist (different parties hold different ones) or one can foreclose another if the evidence becomes conclusive.',
    'The choice of reading determines the constraint''s classification and narrative structure. Punctuated_swap_reading yields ROPE. Overdetermined_composite_reading might yield SCAFFOLD (the Bretton Woods system as a temporary coordination during post-WWII recovery) or TANGLED_ROPE (overdetermined pressures). Triffin_inevitability_reading might yield MOUNTAIN (the Triffin constraint as a physical law of reserve-currency dynamics) or ROPE degraded into PITON (the system could have been reformed but was maintained performatively until collapse). The three readings are not fully commensurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-axis: which sibling reading best captures the causal structure of August 15, 1971?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__punctuated_swap_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t0, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(mone_tr_t0, observed).
narrative_ontology:measurement(mone_tr_t2, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 2, 0.16).
narrative_ontology:measurement_basis(mone_tr_t2, observed).
narrative_ontology:measurement(mone_tr_t4, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement_basis(mone_tr_t4, observed).
narrative_ontology:measurement(mone_tr_t6, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement_basis(mone_tr_t6, observed).
narrative_ontology:measurement(mone_tr_t9, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 9, 0.28).
narrative_ontology:measurement_basis(mone_tr_t9, observed).
narrative_ontology:measurement(mone_tr_t12, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(mone_tr_t12, observed).

% Extraction over time
narrative_ontology:measurement(mone_be_t0, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(mone_be_t0, observed).
narrative_ontology:measurement(mone_be_t2, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement_basis(mone_be_t2, observed).
narrative_ontology:measurement(mone_be_t4, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 4, 0.56).
narrative_ontology:measurement_basis(mone_be_t4, observed).
narrative_ontology:measurement(mone_be_t6, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement_basis(mone_be_t6, observed).
narrative_ontology:measurement(mone_be_t9, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 9, 0.62).
narrative_ontology:measurement_basis(mone_be_t9, observed).
narrative_ontology:measurement(mone_be_t12, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement_basis(mone_be_t12, observed).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t0, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(mone_su_t0, observed).
narrative_ontology:measurement(mone_su_t2, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 2, 0.32).
narrative_ontology:measurement_basis(mone_su_t2, observed).
narrative_ontology:measurement(mone_su_t4, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement_basis(mone_su_t4, observed).
narrative_ontology:measurement(mone_su_t6, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 6, 0.41).
narrative_ontology:measurement_basis(mone_su_t6, observed).
narrative_ontology:measurement(mone_su_t9, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 9, 0.41).
narrative_ontology:measurement_basis(mone_su_t9, observed).
narrative_ontology:measurement(mone_su_t12, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 12, 0.41).
narrative_ontology:measurement_basis(mone_su_t12, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__punctuated_swap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(monetary_anchor_principle__punctuated_swap_reading, 0.18).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__overdetermined_composite_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__triffin_inevitability_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, bretton_woods_dollar_hegemony).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, triffin_dilemma_structural_bind).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'monetary_anchor_principle'. Sibling readings: overdetermined_composite_reading (treats August 15 as overdetermined collapse), triffin_inevitability_reading (treats it as structurally inevitable due to Triffin logic). The three readings differ in their ε values, beneficiary/victim structures, and terminal classifications. Punctuated_swap_reading asserts the decision was contingent; the other readings assert it was determined. All three are valid readings of the same kernel; each compiles to a separate constraint story. The network edges indicate contamination propagation: if one reading's core premises are refuted by new historical evidence or formal analysis, the sibling readings are affected.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monetary_anchor_principle__punctuated_swap_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
