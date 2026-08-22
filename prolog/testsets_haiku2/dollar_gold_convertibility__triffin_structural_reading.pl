% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__triffin_structural_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__triffin_structural_reading, []).

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
 *   constraint_id: dollar_gold_convertibility__triffin_structural_reading
 *   human_readable: Dollar-Gold Convertibility Under Triffin Structural Reading
 *   domain: international_political_economy/monetary_systems
 *
 * SUMMARY:
 *   The Bretton Woods system locked the U.S. dollar to gold at $35/ounce and
 *   all other currencies to the dollar at fixed parities (1944–1971). Under
 *   the Triffin structural reading, this arrangement embodies an inherent
 *   trilemma: the U.S. cannot simultaneously (1) maintain fixed
 *   convertibility of dollars to gold, (2) run deficits to finance global
 *   military and economic leadership, and (3) supply the world's growing
 *   demand for dollar reserves without eventually depleting the gold stock.
 *   Both the U.S. and creditor nations are victims of an impossible choice,
 *   with neither able to exit unilaterally without triggering systemic
 *   collapse. The floating-rate regime that follows is the beneficiary — it
 *   dissolves the constraint by abandoning the fixed gold parity, enabling
 *   independent monetary policy and equilibrating capital flows through
 *   exchange-rate adjustment. This reading claims the constraint is a
 *   tangled_rope: it coordinates trade and capital flows while simultaneously
 *   extracting from both U.S. and creditor seats through the forced
 *   accumulation of increasingly suspicious dollar reserves. The measurement
 *   series track the rising extractiveness and theater ratio as the trilemma
 *   becomes apparent (1960s Triffin publications and gold-market pressures)
 *   and suppression intensifies (institutional suppression of policy
 *   alternatives).
 *
 * KEY AGENTS:
 *   - United States Treasury: institutional agenda-setter and victim — bound by convertibility commitment yet financing deficits that violate it
 *   - Gold creditor nations (UK, France, Germany, Japan): powerful victims — accumulating dollars they increasingly doubt can convert
 *   - Bretton Woods institutional framework: enforcer of convertibility through peer pressure and confidence mechanisms
 *   - Triffin-school economists: analytical observers excluded from operative decision-making despite identifying the trilemma
 *   - Post-Bretton Woods floating regime: beneficiary — emerges as convertibility collapses
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
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__triffin_structural_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__triffin_structural_reading, "Dollar-Gold Convertibility Under Triffin Structural Reading").
narrative_ontology:topic_domain(dollar_gold_convertibility__triffin_structural_reading, "international_political_economy/monetary_systems").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__triffin_structural_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__triffin_structural_reading, '84ecf999-0eac-43f9-9df8-05699ff00f10').
narrative_ontology:cs_kernel_codification('84ecf999-0eac-43f9-9df8-05699ff00f10', formalized).
narrative_ontology:cs_authority_grounding('84ecf999-0eac-43f9-9df8-05699ff00f10', lineage).
narrative_ontology:cs_interpretation_layer_present('84ecf999-0eac-43f9-9df8-05699ff00f10').
narrative_ontology:cs_reading_relation('84ecf999-0eac-43f9-9df8-05699ff00f10', dollar_gold_convertibility__strict_convertibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('84ecf999-0eac-43f9-9df8-05699ff00f10', dollar_gold_convertibility__policy_flexible_reading, influences).
narrative_ontology:cs_axiom('84ecf999-0eac-43f9-9df8-05699ff00f10', foundational, convertibility_mathematically_impossible_trilemma).
narrative_ontology:cs_axiom_status(convertibility_mathematically_impossible_trilemma, holdable).
narrative_ontology:cs_axiom_grounding('84ecf999-0eac-43f9-9df8-05699ff00f10', convertibility_mathematically_impossible_trilemma, empirically_contingent).
narrative_ontology:cs_axiom('84ecf999-0eac-43f9-9df8-05699ff00f10', secondary, deficit_financing_incompatible_with_gold_constraint).
narrative_ontology:cs_axiom_status(deficit_financing_incompatible_with_gold_constraint, holdable).
narrative_ontology:cs_axiom_grounding('84ecf999-0eac-43f9-9df8-05699ff00f10', deficit_financing_incompatible_with_gold_constraint, empirically_contingent).
narrative_ontology:cs_reference_frame('84ecf999-0eac-43f9-9df8-05699ff00f10', bretton_woods_fixed_parity_commitment).
narrative_ontology:cs_drift_state('84ecf999-0eac-43f9-9df8-05699ff00f10', post_1964_trilemma_recognition, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('84ecf999-0eac-43f9-9df8-05699ff00f10', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, united_states_treasury).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, gold_creditor_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates under the Bretton Woods obligation to convert dollars to gold at a fixed parity ($35/oz). As the world's largest economy and central currency issuer, the U.S. Treasury must maintain this convertibility while simultaneously managing domestic monetary policy, deficit spending, and investment needs. The convertibility constraint ties the monetary base to a fixed gold stock, preventing independent macroeconomic action. The institutional identity of U.S. monetary leadership is fused with the dollar's reserve-currency status, making unilateral exit politically costly.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, united_states_treasury, payer,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__triffin_structural_reading, united_states_treasury, agenda_setter).

% Accumulate dollars as reserves under the assumption of fixed convertibility. They rely on the dollar peg to gold as collateral backing their trade surpluses and capital reserves. As the U.S. runs deficits to finance military and welfare expenditures, dollars accumulate abroad. The creditor nations (Britain, France, Germany, Japan) face an impossible choice: hold depreciating dollars or demand gold conversion and deplete the U.S. gold stock, triggering systemic collapse. Their constraint is that demanding conversion accelerates the very collapse they fear.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, gold_creditor_nations, payer,
    powerful, generational, constrained, global).

% The International Monetary Fund and the architecture of fixed parities with the dollar at center enforce the convertibility rule. The system itself becomes the enforcer: any nation attempting to break ranks signals loss of confidence and precipitates runs. The framework suppresses alternative monetary arrangements (sterling blocs, gold standards, bilateral trade) by making U.S. dollar reserves the only acceptable reserve medium.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, bretton_woods_institutional_framework, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(dollar_gold_convertibility__triffin_structural_reading, bretton_woods_institutional_framework).

% Can observe the trilemma: gold stock is finite, dollar liabilities are growing, and the parity is fixed. Speculators and central banks engaged in gold trading accumulate physical gold and demand conversion as confidence declines. Their arbitrage activity accelerates the run on the gold reserve.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, gold_markets_and_speculators, observer,
    organized, biographical, arbitrage, global).

% Economists (led by Robert Triffin) who identify the structural impossibility: a reserve currency cannot simultaneously maintain fixed convertibility, finance deficits, and absorb the world's demand for liquidity. Their analysis is excluded from the operative decision-making apparatus because acknowledging the trilemma would require dismantling the system. The analysis circulates in academic and policy circles but does not drive institutional reform until the system collapses.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, triffin_analysis_observers, excluded,
    analytical, biographical, analytical, global).
narrative_ontology:stakeholder_non_agent(dollar_gold_convertibility__triffin_structural_reading, triffin_analysis_observers).

% The regime that replaces Bretton Woods after 1971 — floating exchange rates, fiat currency, and independent monetary policy for each nation. This regime benefits from the collapse of convertibility because it frees central banks from the gold constraint and enables Keynesian macroeconomic management. The floating regime's emergence is enabled by the structural impossibility of convertibility.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__triffin_structural_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The dollar-gold parity creates a single global numeraire and a hierarchy of stable exchange rates around the dollar, enabling multilateral trade and capital flows without bilateral negotiation. A fixed dollar/gold relationship stabilizes all bilateral rates simultaneously and provides a commitment device: central banks hold dollars because gold convertibility makes them 'as good as gold.'
% TRANSFER_FUNCTION: The constraint transfers monetary sovereignty from creditor nations to the United States. U.S. deficit spending (military and welfare) is financed by exporting dollars that creditors must accept as reserve-equivalent to gold. The U.S. Treasury collects seigniorage — the real value of goods acquired in exchange for nominally convertible dollar liabilities. Creditors accumulate a claim that is nominally convertible but structurally impossible to convert without triggering collapse.
% ABSENT_VOICES: The most economically affected party is absent from the enforcement apparatus: working-class people in creditor and debtor nations whose real consumption is constrained by the system. Developing nations trying to industrialize are also excluded — they cannot access sufficient liquidity under the Bretton Woods hierarchy. Triffin's structural analysis is present as academic discourse but absent from the operational framework that makes policy.
% DISAPPEARANCE_RATIONALE: If the convertibility constraint and its enforcement mechanisms disappeared, the Bretton Woods system would collapse (as it did in 1971). The U.S. would shift to floating-rate fiat currency; capital would immediately redeploy; creditor nations would be forced to hold depreciating dollars or deploy alternative reserve systems. The entire post-war monetary order would reorganize within months. The constraint's disappearance is in fact what defines the Bretton Woods collapse.
% FOUNDING_PROBLEM: After World War II, the global monetary system had no shared numeraire and no mechanism for multilateral settlement. Sterling had collapsed; gold supplies were inelastic; sovereign nations needed a commitment device that would hold without enforcement. The dollar-gold parity promised that the U.S. would not inflate away its currency, making the dollar-denominated system trustworthy as a reserve medium.
% FOUNDING_PROBLEM_CORROBORATION: By the 1960s, economists including Triffin, Kindleberger, and Minsky documented that the founding problem had been solved (the world had recovered, trade was stable, alternatives were available) while the constraint persisted. Archival evidence from central banks shows that creditors understood the trilemma by the 1960s and began requesting conversion. The founding problem (no trusted numeraire, systemic instability) was no longer live; the constraint (fixed convertibility) was by then a pure extraction mechanism.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__triffin_structural_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__triffin_structural_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__triffin_structural_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from 0.35 (1944, coordination-dominated, genuine problem-solving) to 0.82 (1971, pure impossibility rent-collection). The constraint extracts real value because U.S. can finance deficits by exporting dollars, while creditors accumulate a claim they cannot redeem without system collapse. Suppression is high (0.71) because the institutional apparatus (IMF, Fed coordination, central bank peer pressure) actively suppresses policy alternatives that would acknowledge the trilemma — the response to each gold run is to reaffirm commitment, not to reform the system. Theater_ratio rises because by 1968 the IMF's Gold Pool and Two-Tier system is pure performance: the official $35 parity is maintained by central-bank coordination while a parallel market price reflects true scarcity. Accessibility_collapse is moderate (0.64) because creditors always theoretically could demand gold conversion, but doing so triggers the collapse they fear — the alternative of floating rates exists logically but is politically suppressed. Resistance is high (0.73) because the system meets persistent pressure: gold runs (1960, 1968), currency crises, and academic critique accumulate throughout the period.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. Treasury seat, the constraint initially appears as a coordination device and commitment mechanism — it provides legitimacy for dollar leadership. By the 1960s, the same seat experiences it as a straitjacket: the Treasury cannot expand the monetary base without running down gold reserves, and cannot maintain convertibility while financing Vietnam and the Great Society. From the creditor seat, the constraint initially appears as a safe anchor for reserves and trade settlement. By the mid-1960s, it appears as a trap: holding dollars means lending to a deficit-running hegemon at zero interest, trusting a promise (convertibility) that becomes increasingly suspect. The engine computes per-seat types from the structural data: the U.S. Treasury seat should compute as a victim under high suppression; the creditor seat should compute as a payer under identity-lock (they cannot exit without triggering collapse); the post-Bretton Woods regime seat computes as a beneficiary (its freedom of action is enabled by this constraint's collapse).
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. is simultaneously agenda-setter (nominally controlling the parity, choosing deficit spending) and victim (trapped by the convertibility constraint, unable to expand money supply without gold). Directionality for U.S. is high d (0.75+) — it is a net target of the constraint's asymmetry despite setting it, because the constraint binds the U.S. more tightly than it binds willing creditors at the outset. Creditor nations have d = 0.80+ (full targets): they accumulate dollar liabilities they cannot convert without system collapse, and their exit is identity-locked (the international financial system is dollar-based; rejecting dollars means rejecting participation). The post-Bretton Woods regime is a beneficiary with d = 0.0: it gains unconstrained monetary policy and exchange-rate flexibility. No override is needed; the structural derivation captures this.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is a tangled_rope: it coordinates global trade and capital flows (the coordination function), while enforcing asymmetric transfer (the U.S. gains seigniorage, creditors accumulate suspect reserves). The classification prevents mislabeling it as pure coordination (rope) because the transfer is structurally asymmetric and the asymmetry is enforced. By 1965, the founding problem (post-war monetary instability) is dead — the system has achieved its coordination objective. Yet the constraint persists, now functioning as pure extraction: the U.S. runs deficits, exports inflation, and creditors bear the cost. Mandatrophy_resolved should be marked true: the constraint's function has inverted from solving a coordination problem to embodying an impossible extraction mechanism, and the policy community is aware of this inversion (Triffin, 1960; Kindleberger 1968; IMF reform proposals 1960s) while the institutional apparatus suppresses acknowledging it until collapse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_trilemma_necessity,
    'Is the trilemma — the impossibility of simultaneously maintaining convertibility, running deficits, and supplying growing reserve demand — a structural mathematical necessity or a contingent policy choice?',
    'Formal modeling of reserve-currency dynamics under fixed parities, plus counterfactual analysis: had the U.S. maintained balanced budgets and restricted deficit spending, would the system have remained sustainable? Or is the trilemma logically inevitable given the scale of post-war deficits and the growth in international liquidity demand?',
    'If structural necessity: the constraint is a mountain (physically impossible arrangement), not a tangled_rope (choice-dependent extraction). If contingent: the constraint is pure policy rent, classifying as snare. The reading''s entire framing depends on the trilemma being structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_trilemma_necessity, empirical, 'Whether Triffin''s trilemma is logically inevitable or contingent on deficit policy.').

omega_variable(
    knowledge_vs_suppression,
    'By 1960, did policy-makers genuinely not understand the Triffin trilemma, or did they understand it and choose institutional suppression of the analysis to maintain the status quo?',
    'Archival evidence from central bank discussions (Basel meetings, Fed archives), White House economic briefings, and IMF board minutes in the 1960s. Did decision-makers acknowledge the trilemma in private while suppressing it in public? Or was the analysis genuinely unknown at the policy level until 1968?',
    'If knowledge + suppression: suppression is the constraint mechanism, and extractiveness is higher (intentional system-milking). If genuine surprise: extractiveness is lower (emergent impossibility rather than deliberate exploitation). The theater_ratio trajectory depends on when suppression began.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_vs_suppression, empirical, 'Whether policy suppression was deliberate or the trilemma was genuinely unanticipated.').

omega_variable(
    alternative_reform_paths,
    'Were there institutional reforms (SDR-based reserve system, adjustable pegs, multiple reserve currencies) that could have dissolved the trilemma without abandoning convertibility entirely?',
    'Comparative analysis of reform proposals from the IMF and academic sources in the 1960s. Could Keynes''s Bancor, or the Triffin Plan for an international reserve unit, have solved the trilemma while preserving fixed-rate discipline? Or was collapse unavoidable?',
    'If reform paths existed and were chosen away: the constraint is pure extraction (policy choice to maintain privileged position). If collapse was unavoidable: the constraint is a mountain (logical impossibility). If reform paths existed but failed for political reasons: the constraint is a snare (beneficiaries suppressed reform to preserve their rents).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reform_paths, conceptual, 'Whether the Bretton Woods collapse was structurally inevitable or could have been avoided through reform.').

omega_variable(
    committer_frame_ambiguity,
    'Is this reading an accurate characterization of structural economic impossibility, or does it reflect a post-hoc ideological reframing of a system that was politically abandoned for reasons other than structural necessity?',
    'Comparative reading of the three kernel readings: does each generate coherent ε-invariant constraint stories without internal contradiction? Where do their ε values diverge, and do those divergences reflect genuine structural differences or different narrative framings of the same events?',
    'If post-hoc reframing: the reading is contestable (conceptual omega). The Triffin reading''s claim to structural necessity versus the flexible reading''s claim to renegotiable obligation are fundamentally different causal stories, each coherent but incompatible. The engine can flag this as a kernel reading contest (cs_structure.reading_relations) rather than resolving it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_ambiguity, conceptual, 'Whether the Triffin reading captures structural economics or reflects post-hoc ideological reconstruction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__triffin_structural_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1944, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1944, 0.2).
narrative_ontology:measurement_basis(doll_tr_t1944, projected).
narrative_ontology:measurement(doll_tr_t1951, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1951, 0.28).
narrative_ontology:measurement_basis(doll_tr_t1951, observed).
narrative_ontology:measurement(doll_tr_t1958, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1958, 0.35).
narrative_ontology:measurement_basis(doll_tr_t1958, observed).
narrative_ontology:measurement(doll_tr_t1964, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1964, 0.41).
narrative_ontology:measurement_basis(doll_tr_t1964, observed).
narrative_ontology:measurement(doll_tr_t1968, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1968, 0.45).
narrative_ontology:measurement_basis(doll_tr_t1968, observed).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1971, 0.48).
narrative_ontology:measurement_basis(doll_tr_t1971, observed).

% Extraction over time
narrative_ontology:measurement(doll_be_t1944, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1944, 0.35).
narrative_ontology:measurement_basis(doll_be_t1944, projected).
narrative_ontology:measurement(doll_be_t1951, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1951, 0.48).
narrative_ontology:measurement_basis(doll_be_t1951, observed).
narrative_ontology:measurement(doll_be_t1958, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1958, 0.62).
narrative_ontology:measurement_basis(doll_be_t1958, observed).
narrative_ontology:measurement(doll_be_t1964, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1964, 0.71).
narrative_ontology:measurement_basis(doll_be_t1964, observed).
narrative_ontology:measurement(doll_be_t1968, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1968, 0.78).
narrative_ontology:measurement_basis(doll_be_t1968, observed).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1971, 0.82).
narrative_ontology:measurement_basis(doll_be_t1971, observed).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1944, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1944, 0.45).
narrative_ontology:measurement_basis(doll_su_t1944, projected).
narrative_ontology:measurement(doll_su_t1951, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1951, 0.52).
narrative_ontology:measurement_basis(doll_su_t1951, observed).
narrative_ontology:measurement(doll_su_t1958, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1958, 0.6).
narrative_ontology:measurement_basis(doll_su_t1958, observed).
narrative_ontology:measurement(doll_su_t1964, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1964, 0.66).
narrative_ontology:measurement_basis(doll_su_t1964, observed).
narrative_ontology:measurement(doll_su_t1968, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1968, 0.69).
narrative_ontology:measurement_basis(doll_su_t1968, observed).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1971, 0.71).
narrative_ontology:measurement_basis(doll_su_t1971, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__triffin_structural_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(dollar_gold_convertibility__triffin_structural_reading, 0.18).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility__strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility__policy_flexible_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, bretton_woods_institutional_enforcement).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, seigniorage_extraction__reserve_currency_rent).

% DUAL FORMULATION NOTE:
% The dollar-gold convertibility kernel decomposes into three structurally distinct constraint stories corresponding to three readings: (1) strict_convertibility_reading — the constraint as binding legal obligation; (2) policy_flexible_reading — the constraint as renegotiable commitment subordinate to domestic stability; (3) triffin_structural_reading (this story) — the constraint as inherently unsustainable trilemma. Each reading instantiates a different ε-value and different beneficiary/victim structure. The Triffin reading claims highest extractiveness (0.82) and frames both U.S. and creditors as victims of an impossible system. The readings coexist as competing interpretations held by different parties in the policy debate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dollar_gold_convertibility__triffin_structural_reading, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
