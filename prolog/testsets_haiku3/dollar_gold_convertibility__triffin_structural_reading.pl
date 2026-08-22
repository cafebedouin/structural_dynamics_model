% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__triffin_structural_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility_triffin_structural_reading, []).

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
 *   constraint_id: dollar_gold_convertibility__triffin_structural_reading
 *   human_readable: Dollar-Gold Convertibility: Triffin Structural Impossibility Reading
 *   domain: international_political_economy/monetary_systems/international_law
 *
 * SUMMARY:
 *   The dollar-gold convertibility commitment (Article IV of the Bretton
 *   Woods Agreement, 1944) anchored post-war monetary stability. By the
 *   mid-1960s, economist Robert Triffin diagnosed a structural impossibility:
 *   as the U.S. ran persistent deficits to finance growth and geopolitical
 *   commitments, dollars accumulated in foreign central banks faster than
 *   U.S. gold reserves could cover. The U.S. faced an impossible trilemma —
 *   defend the peg and choke domestic growth, or permit monetary expansion
 *   and watch gold reserves drain. Creditor nations faced the complementary
 *   trap: accept depreciating dollars or withdraw gold and trigger collapse.
 *   This reading frames convertibility not as a binding legal obligation
 *   (strict reading) or as a policy tool (flexible reading), but as a
 *   structural design flaw whose internal contradictions made it
 *   unsustainable by construction. High extractiveness for both victim
 *   parties; high suppression because the contradiction was widely diagnosed
 *   but institutional lock-in prevented exit; theater increasing over time as
 *   financial engineering (London Gold Pool, two-tier market) substituted for
 *   real policy adjustment.
 *
 * KEY AGENTS:
 *   - United States Monetary Authority (trapped issuer, identity-locked to dollar reserve role)
 *   - Creditor Nations (France, Germany, UK — constrained holders of depreciating reserves)
 *   - Triffin and Bretton Woods critics (external analysts documenting the contradiction)
 *   - Speculative capital markets (rational responders to the visible dilemma)
 *   - Floating Exchange Regime (structural beneficiary of the constraint's collapse)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, 0.82).
domain_priors:suppression_score(dollar_gold_convertibility__triffin_structural_reading, 0.71).
domain_priors:theater_ratio(dollar_gold_convertibility__triffin_structural_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__triffin_structural_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__triffin_structural_reading, "Dollar-Gold Convertibility: Triffin Structural Impossibility Reading").
narrative_ontology:topic_domain(dollar_gold_convertibility__triffin_structural_reading, "international_political_economy/monetary_systems/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__triffin_structural_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__triffin_structural_reading, 'e4c3e48e-ccc7-4a43-82a9-d087bdcc1640').
narrative_ontology:cs_kernel_codification('e4c3e48e-ccc7-4a43-82a9-d087bdcc1640', formalized).
narrative_ontology:cs_authority_grounding('e4c3e48e-ccc7-4a43-82a9-d087bdcc1640', extraction).
narrative_ontology:cs_interpretation_layer_present('e4c3e48e-ccc7-4a43-82a9-d087bdcc1640').
narrative_ontology:cs_reading_relation('e4c3e48e-ccc7-4a43-82a9-d087bdcc1640', dollar_gold_convertibility__strict_convertibility_reading, forecloses).
narrative_ontology:cs_reading_relation('e4c3e48e-ccc7-4a43-82a9-d087bdcc1640', dollar_gold_convertibility__policy_flexible_reading, influences).
narrative_ontology:cs_axiom('e4c3e48e-ccc7-4a43-82a9-d087bdcc1640', foundational, convertibility_structurally_unsustainable).
narrative_ontology:cs_axiom_status(convertibility_structurally_unsustainable, holdable).
narrative_ontology:cs_axiom_grounding('e4c3e48e-ccc7-4a43-82a9-d087bdcc1640', convertibility_structurally_unsustainable, empirically_contingent).
narrative_ontology:cs_axiom('e4c3e48e-ccc7-4a43-82a9-d087bdcc1640', foundational, reserve_currency_deficit_trilemma_inevitable).
narrative_ontology:cs_axiom_status(reserve_currency_deficit_trilemma_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('e4c3e48e-ccc7-4a43-82a9-d087bdcc1640', reserve_currency_deficit_trilemma_inevitable, empirically_contingent).
narrative_ontology:cs_reference_frame('e4c3e48e-ccc7-4a43-82a9-d087bdcc1640', bretton_woods_fixed_peg_with_gold_redemption).
narrative_ontology:cs_drift_state('e4c3e48e-ccc7-4a43-82a9-d087bdcc1640', post_1960_triffin_diagnosis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e4c3e48e-ccc7-4a43-82a9-d087bdcc1640', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, floating_exchange_regime_post_1973).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, united_states_monetary_authority).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, creditor_nations_under_bretton_woods).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, speculative_capital_markets).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__triffin_structural_reading, triffin_dilemma).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__triffin_structural_reading, impossible_trinity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commits to redeem dollars for gold at $35/oz under Article IV, anchoring the international monetary system. As deficits accumulate, the U.S. Treasury and Federal Reserve face a bind: defending the peg requires reducing money supply and growth (politically impossible), while permitting expansion causes gold outflows. The identity lock is institutional — the dollar's reserve-currency role and the U.S.'s postwar hegemonic status make the commitment appear inescapable, yet the commitment becomes progressively impossible to fulfill. The U.S. does not name itself as a victim; instead, it frames the problem as external (other nations' unwillingness to hold dollars). Yet the Triffin reading insists that the U.S. is as trapped as creditors — the trap is structural, not someone's malice.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, united_states_monetary_authority, payer,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__triffin_structural_reading, united_states_monetary_authority, agenda_setter).

% France, Germany, UK, and other creditor nations hold dollar reserves from their postwar recovery and trade surpluses. They have the legal right to convert dollars to gold at the fixed rate. But converting en masse would drain U.S. gold reserves and force devaluation, collapsing the system they depend on. So they forbear — they hold depreciating dollars, watching their real wealth erode. Their exit is constrained: unilateral conversion triggers panic and losses; coordinated exit to a new reserve regime is politically impossible because no agreement on an alternative exists. They are trapped in dollar dependence not by U.S. coercion but by the structure of the dilemma itself.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, creditor_nations_under_bretton_woods, payer,
    powerful, biographical, constrained, global).

% Private investors and currency speculators observe the visible contradiction: as gold reserves decline and dollar holdings expand, the peg becomes fragile. They rationally convert dollars to gold in anticipation of devaluation. Their rational response to the structural contradiction accelerates the constraint's collapse (the 1968 crisis in the London gold pool, the 1971 final rush). They are payers because their mobile exit behavior exacerbates the dilemma for official parties, yet their individual rationality is the correct response to the structural impossibility.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, speculative_capital_markets, payer,
    organized, immediate, mobile, global).

% Keynes, White, and the negotiators who framed the 1944 agreement. They designed the convertibility commitment in good faith to anchor stability and prevent a return to 1930s currency chaos. They did not anticipate the magnitude or duration of U.S. deficits. By the 1960s, many of the original architects (or their intellectual heirs) recognized the structural flaw — Keynes had died in 1946, but his analysis of the gold standard's constraints proved prophetic. The architects' role transitions from agenda-setter (1944) to observer (1960s onward).
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, bretton_woods_architects, agenda_setter,
    institutional, generational, analytical, global).

% Economist Robert Triffin, along with Kindleberger, Machlup, and other international economists, diagnose the structural impossibility (Triffin's 1960 book is the landmark). They make the contradiction visible and analyzable. They are observers rather than parties, but their analysis becomes ammunition for both official parties and speculators. Triffin himself advocates for reform (the Triffin Plan), but his role remains analytical — he does not hold reserves, does not set policy, does not directly experience the constraint, yet his work reshapes how all parties perceive it.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, triffin_and_bretton_woods_critics, observer,
    analytical, generational, analytical, global).

% After the 1973 Nixon Shock dismantles the peg, floating exchange rates become the new coordinative mechanism for international monetary exchange. The floating regime has no direct extraction: it does not run deficits, does not hold gold, does not commit to redemption. It emerges as the structural beneficiary because the constraint's unsustainability necessitates the shift. In a sense, the floating regime benefits from the constraint's failure — its logic is vindicated (markets allocate exchange rates more flexibly than fixed pegs) by the impossible choice that the peg presented.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, floating_exchange_regime_post_1973, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(dollar_gold_convertibility__triffin_structural_reading, floating_exchange_regime_post_1973).

% Sterling (UK), franc (France), mark (Germany), and yen (Japan) could theoretically diversify as reserves or compete against the dollar. But under Bretton Woods, the dollar's legal supremacy locks them out. The UK remains dependent despite its own postwar weakness. France, under de Gaulle, explicitly calls for alternatives but lacks the economic weight to establish them. Germany and Japan, rebuilt under U.S. security umbrellas, are trapped in dollar dependence. They would voice objections if admitted to redesign the system, but the structure excludes them — they are not at the negotiation table, and their exit into alternative reserves is blocked by the same peg that locks in everyone else.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, competing_reserve_currency_claimants, excluded,
    powerful, biographical, trapped, global).

% The IMF was designed to manage balance-of-payments adjustment and oversee Bretton Woods. By the 1960s, it observes (via Triffin, staff analysis, and member consultations) that the peg is unsustainable. But the IMF structurally cannot mandate solutions: it lacks authority over the U.S. (which is the Board's largest shareholder and controls the veto), and it cannot force creditor nations to abandon gold redemption or hold depreciating dollars indefinitely. The IMF remains an observer and facilitator, mediating negotiations (standby credits, swap agreements) that temporarily postpone the collapse but cannot resolve the underlying contradiction.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__triffin_structural_reading, floating_exchange_regime_post_1973).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__triffin_structural_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bretton Woods convertibility was designed to anchor the post-war international monetary system by fixing the dollar-gold parity at $35 per ounce and committing the U.S. to redeem dollars in gold on demand. This provides a stable numeraire for international trade and capital flows, and underpins confidence that the dollar retains its value across time and borders. The convertibility commitment solves the coordination problem of currency instability that plagued the 1930s and the 1940s.
% TRANSFER_FUNCTION: The constraint redistributes monetary-policy autonomy and seigniorage: the fixed peg removes the U.S.'s ability to expand money supply freely (each dollar issued must be backed by potential redemption in gold), while it allows creditor nations to accumulate dollar reserves that accrue them seigniorage-like benefits (they hold assets denominated in another nation's currency). But as U.S. deficits persist, the transfer reverses — creditors accumulate paper that loses real value as inflation erodes the dollar's purchasing power, and the U.S. must choose between devaluing (breaking the peg, imposing losses on creditors) or deflating (choking domestic growth).
% ABSENT_VOICES: Countries that would benefit from reserve-currency competition (France explicitly, Germany and Japan implicitly) and floating-rate advocates in the academic community. They are structurally excluded from the negotiation table because Bretton Woods is presented as legally irreversible — the IMF Articles of Agreement frame the peg as binding law. Developing nations dependent on dollar stability are not consulted but bear the risk of system collapse.
% DISAPPEARANCE_RATIONALE: The constraint's collapse rearranged the entire international monetary order: the fixed peg dissolved overnight in August 1971 (Nixon Shock); floating regimes emerged within months; the dollar retained reserve-currency status but without the gold convertibility commitment; capital flows reorganized around exchange-rate hedging and forward markets. The constraint's persistence 1944-1973 depended on both the U.S. and creditor nations forbearing from actions that would trigger collapse. Once the contradiction became undeniable (gold reserves fell below $12 billion, dollar holdings held by foreigners exceeded gold coverage by 3:1), forbearance became impossible and the system rearranged rapidly.
% FOUNDING_PROBLEM: The Bretton Woods architects faced a real founding problem in 1944: the 1930s and early 1940s had witnessed currency chaos, competitive devaluations, trade collapse, and lack of coordination. The fixed peg and convertibility were designed to restore confidence, anchor expectations, and enable stable trade and capital flows without the self-defeating devaluations that had plagued the prior era.
% FOUNDING_PROBLEM_CORROBORATION: By 1965, independent economists (Triffin, Kindleberger, Machlup, Hirsch, and others outside the beneficiary camp) demonstrated that the founding problem had been solved: international trade was booming, private capital flowed freely, and confidence in the dollar was high — the very conditions Bretton Woods was designed to create had been achieved. Yet the constraint persisted for another 8 years (1965-1973). Robert Triffin's 1960 book 'Gold and the Dollar Crisis' and his subsequent testimony before the U.S. Congress and international bodies documented that the founding mandate (restore confidence) was fulfilled and the commitment was now sustaining nothing but institutional inertia. The 1971 Smithsonian Agreement and Congressional debates confirm that even official parties recognized the mandate as dead; what persisted was the structure itself, not a living problem.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__triffin_structural_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__triffin_structural_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__triffin_structural_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dollar_gold_convertibility__triffin_structural_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__triffin_structural_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 (1944, the peg appears stable) to 0.82 (1973, collapse imminent). The trajectory reflects the accumulation of U.S. deficits relative to gold reserves: initially, the constraint is benign because the problem is latent; by 1960 (after Marshall Plan and Cold War buildup), the problem is visible but tolerated because alternatives look worse; by 1970, the constraint is actively grinding both parties — U.S. cannot expand without losing gold, creditors cannot act without triggering panic. Theater_ratio rises from 0.05 to 0.48: early Bretton Woods is functional (real gold backing, real capital flows settle at the peg); by 1968, the London Gold Pool (central banks cooperating to defend the peg while private markets trade at a premium) is pure theater — the two-tier system admits the official peg is no longer the real price. Suppression increases because both the U.S. and creditors face escalating pressure to act against the peg (U.S. to devalue, creditors to convert), yet both suppress the action because the alternative (system collapse) appears worse. The coercion grid shows that stakes inflation is concentrated at the structural and organizational levels (Fed policy, central bank coordination) but individual speculators have relatively low stakes — yet their mobile, rational response to structural pressure accelerates collapse. By 1973, resistance at the structural level reaches 0.88 because the constraint's logic has become openly contradictory and both sides act to escape it.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. seat: the constraint is increasingly unilateral extraction imposed by creditor nations (they hoard dollars, then convert to gold, draining reserves — from this view, creditors are the extractors). From the creditor seat: the constraint is imposition by the U.S. (it runs deficits and inflates the dollar, then forces creditors to hold paper that depreciates — from this view, the U.S. is the extractor). This reading resolves the perspectival asymmetry by locating the extraction not in agency but in structure: the convertibility rule itself extracts from both parties by locking both into an impossible situation. Neither party benefits; both pay (hence tangled_rope, not snare). The floating regime that replaces it is the structural beneficiary because floating removes both constraints at once. The engine should compute divergent types from the two victim seats because they experience the extraction mechanism through different causal paths, even though both are trapped.
 *
 * DIRECTIONALITY LOGIC:
 *   United States: identity-locked to the dollar reserve role (cannot exit without redefining itself as non-hegemonic power), high extractiveness (must choose between monetary autonomy and gold reserves, can have neither fully). Directionality should be near 1.0 (full target). Creditor Nations: constrained exit (cannot diversify reserves without coordination because unilateral moves trigger panic; cannot stay because dollars depreciate). Directionality near 1.0 (full target). Floating regime post-1973: structured beneficiary (the constraint's collapse vindicates its logic and clears the field for floating; it has zero extraction cost because it does not run up against the gold constraint). Directionality should be near 0.0 (beneficiary, not paying). No overrides needed — the structural derivation should land correctly because the beneficiary/victim declarations are clean and the exits are accurately characterized.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was real in 1944 (currency chaos of the 1930s). By 1965, it was solved: international trade thrived, capital flowed, confidence was high in the dollar. Yet the Bretton Woods commitment persisted for another 8 years (1965-1973) because the institutional mechanism created to solve a real problem had become inescapable even though the problem was gone. This is exactly the mandatrophy condition: the mandate (restore confidence and anchor the monetary system) was fulfilled, but the constraint persisted through institutional inertia and lock-in. The disappearance_verdict is world_rearranges and founding_problem_status is dead — these signal mandatrophy. What prevents earlier recognition of mandate obsolescence is the theater: policy makers, central bankers, and creditor governments engage in financial engineering (two-tier gold markets, swap lines, IMF negotiations) that maintains the appearance of control while the fundamental contradiction grows. Once Triffin's structural diagnosis becomes consensus (by 1970), the constraint is recognized as imposing costs on both parties for a problem that no longer exists — yet it persists another 3 years until the final gold run forces the Nixon Shock. The theater_ratio rising toward 0.48 by 1973 reflects this disconnect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    triffin_dilemma_necessity_vs_contingency,
    'Was Triffin''s diagnosed contradiction an inevitable structural consequence of the Bretton Woods design, or a contingent result of specific historical conditions (U.S. Cold War spending, postwar reconstruction loans, unexpected persistence of U.S. deficits)?',
    'Comparative monetary-system analysis: would a different initial distribution of gold reserves, or different geopolitical commitments by the U.S., have avoided the trilemma? Counterfactual modeling of sustainable deficit levels.',
    'If inevitable: the constraint was doomed from inception and any reading that treats it as workable is systemically naive. If contingent: the constraint''s failure reflects policy choices, not design flaw, and the strict or flexible readings gain credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_dilemma_necessity_vs_contingency, empirical, 'Whether the convertibility impossibility was structural or historical accident.').

omega_variable(
    double_victimhood_asymmetry,
    'Are the U.S. and creditor nations genuinely symmetrical victims of the same constraint, or does one party benefit more from the constraint''s persistence or collapse?',
    'Decompose the effect of the constraint on each party''s actual policy options and outcomes: Did U.S. deficits finance goods the nation genuinely needed, or did the privilege of the reserve currency let the U.S. externalize costs? Did creditor nations accumulate dollar reserves voluntarily (because dollars were useful) or under pressure (because alternatives were blocked)?',
    'If asymmetric (U.S. benefits despite the peg''s costs): reclassify as snare where the U.S. extracts privilege from the constraint''s logic. If symmetric: the tangled_rope framing holds. The beneficiary classification (floating regime) remains correct either way.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(double_victimhood_asymmetry, empirical, 'Whether both victim parties are equally victimized by the constraint.').

omega_variable(
    institutional_lock_in_vs_rational_persistence,
    'Why did the constraint persist 1965-1973 even after Triffin''s diagnosis was widely accepted? Was this institutional inertia and theater (mandate obsolete but politically impossible to acknowledge), or rational forbearance because the available alternatives looked worse?',
    'Historical analysis of decision-making by U.S. Treasury, Federal Reserve, and creditor central banks during 1965-1971: explicit statements about whether the constraint was seen as sustainable, and what alternatives were rejected.',
    'If theater dominates: the story is a piton candidate masquerading as rope, and the mandate obsolescence thesis is vindicated. If rational forbearance: the constraint was extractive by collective choice (both parties preferred the pain of Bretton Woods to the uncertainty of alternatives), not by structural trap — different classification implications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_lock_in_vs_rational_persistence, empirical, 'Whether constraint persistence 1965-73 was theater or rational choice under uncertainty.').

omega_variable(
    kernel_reading_under_determination,
    'Does Bretton Woods Article IV instantiate a single constraint (the peg) or does the reading shape what counts as the constraint? Is the constraint the fixed price ($35/oz), the U.S. commitment to redeem, or the entire system''s coordination logic?',
    'Prolog test: if we fix the peg but remove the redemption commitment, does extractiveness change? If we keep the commitment but allow the price to float, does extractiveness change? Which change is larger?',
    'If the peg is the constraint: strict readings are correct and the structure can be reformed by price adjustment. If the commitment is the constraint: the reading matters (flexible reading allows circumvention; structural reading says circumvention is temporally bounded). The engine''s per-seat classification depends on which framing the structural data captures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_under_determination, conceptual, 'What the constraint''s identity is under different framings of the kernel.').

omega_variable(
    beneficiary_classification_post_collapse,
    'Is the ''floating exchange regime post-1973'' a true structural beneficiary (it actively exists because the constraint collapsed), or is it a counterfactual alternative that would have existed anyway?',
    'Counterfactual: if the Bretton Woods peg had remained technically feasible (e.g., if the U.S. had restricted deficits sufficiently), would the world have eventually adopted floating rates anyway, or only adopted them because the peg became unsustainable?',
    'If the floating regime required the peg''s collapse: it is a beneficiary of the constraint''s unsustainability (benefits from the constraint''s failure). If floating was inevitable regardless: the float is not truly a beneficiary, and the constraint''s failure is not vindicating but just opening space for inevitable change. Affects whether the engine classifies floating as extracting benefit from the constraint or as independent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_classification_post_collapse, conceptual, 'Whether the floating regime is a beneficiary of the constraint or an inevitable alternative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__triffin_structural_reading, 1944, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1944, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1944, 0.05).
narrative_ontology:measurement_basis(doll_tr_t1944, projected).
narrative_ontology:measurement(doll_tr_t1950, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement_basis(doll_tr_t1950, observed).
narrative_ontology:measurement(doll_tr_t1960, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1960, 0.18).
narrative_ontology:measurement_basis(doll_tr_t1960, observed).
narrative_ontology:measurement(doll_tr_t1965, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1965, 0.32).
narrative_ontology:measurement_basis(doll_tr_t1965, observed).
narrative_ontology:measurement(doll_tr_t1970, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1970, 0.42).
narrative_ontology:measurement_basis(doll_tr_t1970, observed).
narrative_ontology:measurement(doll_tr_t1973, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1973, 0.48).
narrative_ontology:measurement_basis(doll_tr_t1973, observed).

% Extraction over time
narrative_ontology:measurement(doll_be_t1944, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1944, 0.15).
narrative_ontology:measurement_basis(doll_be_t1944, projected).
narrative_ontology:measurement(doll_be_t1950, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1950, 0.22).
narrative_ontology:measurement_basis(doll_be_t1950, observed).
narrative_ontology:measurement(doll_be_t1960, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1960, 0.48).
narrative_ontology:measurement_basis(doll_be_t1960, observed).
narrative_ontology:measurement(doll_be_t1965, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1965, 0.65).
narrative_ontology:measurement_basis(doll_be_t1965, observed).
narrative_ontology:measurement(doll_be_t1970, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1970, 0.76).
narrative_ontology:measurement_basis(doll_be_t1970, observed).
narrative_ontology:measurement(doll_be_t1973, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1973, 0.82).
narrative_ontology:measurement_basis(doll_be_t1973, observed).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1944, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1944, 0.35).
narrative_ontology:measurement_basis(doll_su_t1944, projected).
narrative_ontology:measurement(doll_su_t1950, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1950, 0.42).
narrative_ontology:measurement_basis(doll_su_t1950, observed).
narrative_ontology:measurement(doll_su_t1960, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement_basis(doll_su_t1960, observed).
narrative_ontology:measurement(doll_su_t1965, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1965, 0.64).
narrative_ontology:measurement_basis(doll_su_t1965, observed).
narrative_ontology:measurement(doll_su_t1970, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1970, 0.68).
narrative_ontology:measurement_basis(doll_su_t1970, observed).
narrative_ontology:measurement(doll_su_t1973, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1973, 0.71).
narrative_ontology:measurement_basis(doll_su_t1973, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1944, tn=1973
narrative_ontology:measurement(doll_grid_01, dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse(class), 1944, 0.55).
narrative_ontology:measurement(doll_grid_02, dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse(class), 1973, 0.78).
narrative_ontology:measurement(doll_grid_03, dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse(individual), 1944, 0.35).
narrative_ontology:measurement(doll_grid_04, dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse(individual), 1973, 0.62).
narrative_ontology:measurement(doll_grid_05, dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse(organizational), 1944, 0.68).
narrative_ontology:measurement(doll_grid_06, dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse(organizational), 1973, 0.82).
narrative_ontology:measurement(doll_grid_07, dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse(structural), 1944, 0.72).
narrative_ontology:measurement(doll_grid_08, dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse(structural), 1973, 0.88).
narrative_ontology:measurement(doll_grid_09, dollar_gold_convertibility__triffin_structural_reading, resistance(class), 1944, 0.08).
narrative_ontology:measurement(doll_grid_10, dollar_gold_convertibility__triffin_structural_reading, resistance(class), 1973, 0.72).
narrative_ontology:measurement(doll_grid_11, dollar_gold_convertibility__triffin_structural_reading, resistance(individual), 1944, 0.05).
narrative_ontology:measurement(doll_grid_12, dollar_gold_convertibility__triffin_structural_reading, resistance(individual), 1973, 0.62).
narrative_ontology:measurement(doll_grid_13, dollar_gold_convertibility__triffin_structural_reading, resistance(organizational), 1944, 0.18).
narrative_ontology:measurement(doll_grid_14, dollar_gold_convertibility__triffin_structural_reading, resistance(organizational), 1973, 0.82).
narrative_ontology:measurement(doll_grid_15, dollar_gold_convertibility__triffin_structural_reading, resistance(structural), 1944, 0.15).
narrative_ontology:measurement(doll_grid_16, dollar_gold_convertibility__triffin_structural_reading, resistance(structural), 1973, 0.88).
narrative_ontology:measurement(doll_grid_17, dollar_gold_convertibility__triffin_structural_reading, stakes_inflation(class), 1944, 0.18).
narrative_ontology:measurement(doll_grid_18, dollar_gold_convertibility__triffin_structural_reading, stakes_inflation(class), 1973, 0.72).
narrative_ontology:measurement(doll_grid_19, dollar_gold_convertibility__triffin_structural_reading, stakes_inflation(individual), 1944, 0.08).
narrative_ontology:measurement(doll_grid_20, dollar_gold_convertibility__triffin_structural_reading, stakes_inflation(individual), 1973, 0.45).
narrative_ontology:measurement(doll_grid_21, dollar_gold_convertibility__triffin_structural_reading, stakes_inflation(organizational), 1944, 0.32).
narrative_ontology:measurement(doll_grid_22, dollar_gold_convertibility__triffin_structural_reading, stakes_inflation(organizational), 1973, 0.85).
narrative_ontology:measurement(doll_grid_23, dollar_gold_convertibility__triffin_structural_reading, stakes_inflation(structural), 1944, 0.28).
narrative_ontology:measurement(doll_grid_24, dollar_gold_convertibility__triffin_structural_reading, stakes_inflation(structural), 1973, 0.91).
narrative_ontology:measurement(doll_grid_25, dollar_gold_convertibility__triffin_structural_reading, suppression(class), 1944, 0.22).
narrative_ontology:measurement(doll_grid_26, dollar_gold_convertibility__triffin_structural_reading, suppression(class), 1973, 0.68).
narrative_ontology:measurement(doll_grid_27, dollar_gold_convertibility__triffin_structural_reading, suppression(individual), 1944, 0.12).
narrative_ontology:measurement(doll_grid_28, dollar_gold_convertibility__triffin_structural_reading, suppression(individual), 1973, 0.58).
narrative_ontology:measurement(doll_grid_29, dollar_gold_convertibility__triffin_structural_reading, suppression(organizational), 1944, 0.35).
narrative_ontology:measurement(doll_grid_30, dollar_gold_convertibility__triffin_structural_reading, suppression(organizational), 1973, 0.72).
narrative_ontology:measurement(doll_grid_31, dollar_gold_convertibility__triffin_structural_reading, suppression(structural), 1944, 0.38).
narrative_ontology:measurement(doll_grid_32, dollar_gold_convertibility__triffin_structural_reading, suppression(structural), 1973, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__triffin_structural_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(dollar_gold_convertibility__triffin_structural_reading, 0.18).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, bretton_woods_architecture_symmetric_pair_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, post_1973_floating_hegemonic_stability_reading).

% DUAL FORMULATION NOTE:
% The dollar-gold convertibility constraint family comprises three readings of a single kernel (Article IV of the Bretton Woods Agreement). This reading (triffin_structural_reading) frames convertibility as an internally contradictory structure whose logical impossibility becomes manifest over time. It stands in conceptual contrast to strict_convertibility_reading (treats the peg as binding law) and policy_flexible_reading (treats the peg as a conditional tool). All three readings share the same referent (the actual convertibility rule and its operation 1944-1973) but differ in their treatment of whether the rule was ever sustainable and who bears costs. The network edges reflect causal dependency: the structural impossibility Triffin diagnosed was the primary driver of the system's evolution toward floating rates (1973 Nixon Shock).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
