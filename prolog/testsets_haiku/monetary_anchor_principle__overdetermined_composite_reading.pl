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
 *   human_readable: Gold Standard Collapse via Overdetermined Composite Pressures
 *   domain: economic/political/international
 *
 * SUMMARY:
 *   The Bretton Woods gold-standard system constrained US fiscal policy by
 *   requiring gold backing for the dollar. By 1960, multiple structural
 *   pressures converged: the Triffin dilemma (reserve issuers must run
 *   deficits to supply global liquidity, exhausting gold reserves); Vietnam
 *   War deficits (reaching $25 billion annually by 1968); Keynesian policy
 *   consensus (demand-management doctrine rejecting monetary discipline); and
 *   technological capital mobility (enabling rapid capital flight as
 *   confidence eroded). This reading interprets the 1971 collapse not as a
 *   discrete policy choice or even as a simple Triffin-driven inevitability,
 *   but as the overdetermined outcome of four structural forces that made any
 *   other outcome implausible by late 1960s. Each pressure alone might have
 *   been survivable; together they forced the transition. The constraint
 *   operated as a tangled rope: it coordinated global exchange rates (real
 *   function) while extracting fiscal discipline from the US (asymmetric
 *   burden), and its persistence depended on active suppression (London Gold
 *   Pool defensive coordination, capital controls, coordination with allied
 *   central banks to prevent gold runs).
 *
 * KEY AGENTS:
 *   - US fiscal administration: runs persistent deficits from Vietnam War and social spending; benefits from constraint abandonment
 *   - Foreign central banks: hold dollars and face redemption-timing dilemma; constrained exit
 *   - Keynesian policy establishment: doctrine privileged by constraint abandonment; vindicated proposition
 *   - Capital mobility operators: excluded from formal policy but their exits accelerate breakdown; technological infrastructure matures over interval
 *   - Triffin dilemma mechanics: mountain upstream (logical necessity of reserve-issuer deficits); feeds into policy entanglement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, 0.82).
domain_priors:suppression_score(monetary_anchor_principle__overdetermined_composite_reading, 0.71).
domain_priors:theater_ratio(monetary_anchor_principle__overdetermined_composite_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(monetary_anchor_principle__overdetermined_composite_reading, "Gold Standard Collapse via Overdetermined Composite Pressures").
narrative_ontology:topic_domain(monetary_anchor_principle__overdetermined_composite_reading, "economic/political/international").

domain_priors:requires_active_enforcement(monetary_anchor_principle__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__overdetermined_composite_reading, '6f061de9-d64a-46f0-a199-9301bca35738').
narrative_ontology:cs_kernel_codification('6f061de9-d64a-46f0-a199-9301bca35738', fixed_text).
narrative_ontology:cs_authority_grounding('6f061de9-d64a-46f0-a199-9301bca35738', extraction).
narrative_ontology:cs_interpretation_layer_present('6f061de9-d64a-46f0-a199-9301bca35738').
narrative_ontology:cs_reading_relation('6f061de9-d64a-46f0-a199-9301bca35738', monetary_anchor_principle__punctuated_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('6f061de9-d64a-46f0-a199-9301bca35738', monetary_anchor_principle__triffin_inevitability_reading, influences).
narrative_ontology:cs_axiom('6f061de9-d64a-46f0-a199-9301bca35738', foundational, structural_overdetermination_hypothesis).
narrative_ontology:cs_axiom_status(structural_overdetermination_hypothesis, holdable).
narrative_ontology:cs_axiom_grounding('6f061de9-d64a-46f0-a199-9301bca35738', structural_overdetermination_hypothesis, empirically_contingent).
narrative_ontology:cs_axiom('6f061de9-d64a-46f0-a199-9301bca35738', secondary, multiple_causal_pathways_sufficiency).
narrative_ontology:cs_axiom_status(multiple_causal_pathways_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('6f061de9-d64a-46f0-a199-9301bca35738', multiple_causal_pathways_sufficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('6f061de9-d64a-46f0-a199-9301bca35738', bretton_woods_gold_standard_commitment).
narrative_ontology:cs_drift_state('6f061de9-d64a-46f0-a199-9301bca35738', late_1960s_crisis_point, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('6f061de9-d64a-46f0-a199-9301bca35738', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, fiscal_state_capacity).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policy_consensus).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, monetary_discipline).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, reserve_currency_credibility).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policy_establishment).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, foreign_central_banks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs persistent deficits to fund Vietnam War escalation and domestic social programs. Gold standard requires discipline in fiscal spending; the constraint is that foreign dollar claims can be redeemed in gold at fixed parity. By 1968, the combination of war spending, social spending, and commitment to Keynesian full-employment policy makes the constraint binding—the administration must either cut spending, raise taxes, or abandon the gold anchor. They choose to preserve fiscal capacity by defending the dollar price of gold through defensive coordination (London Gold Pool) until abandonment becomes unavoidable.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, us_fiscal_administration, agenda_setter,
    institutional, biographical, arbitrage, global).

% Hold dollar reserves and gold conversion rights. As US fiscal deficits accumulate, they face a dilemma: redeem dollars for gold (accelerating reserve depletion and forcing abandonment), or hold dollars and absorb inflation risk. Their constraint exit is narrow—they depend on dollar-denominated global trade, cannot easily hold alternative reserves in the 1960s, and face political pressure at home from constituents questioning why they are financing US war spending. By late 1960s, some (France under de Gaulle) convert dollars to gold explicitly to pressure the system; others hold passively while the arrangement deteriorates.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, foreign_central_banks, payer,
    institutional, generational, constrained, global).

% The rule that fiscal policy must be constrained by monetary backing (the gold standard commitment). This regime is not an actor but a structural principle the arrangement enforces. As the constraint's burden grows, the pressure to abandon it intensifies. By 1970, the principle is overridden—floating rates and fiat currency become normal.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, monetary_discipline_regime, payer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(monetary_anchor_principle__overdetermined_composite_reading, monetary_discipline_regime).

% The trust that dollar reserves are 'as good as gold.' As gold reserves fall and the redemption ratio deteriorates, this credibility erodes. The constraint's persistence depends on belief in fixed parity; once that belief breaks, the transition accelerates. By August 1971, the formal abandonment is almost anticlimactic—market expectations have already shifted.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, reserve_currency_credibility, payer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(monetary_anchor_principle__overdetermined_composite_reading, reserve_currency_credibility).

% Institutional investors, multinational corporations, and arbitrage traders who have the capacity to move capital across borders seeking higher returns or safety. They are excluded from the formal policy-making process but their actions (moving capital out of dollars when confidence is shaken, triggering capital flight) are a major driver of the system's breakdown. Their mobility is technically possible under Bretton Woods but restricted by capital controls; as controls weaken in the 1960s, their exit accelerates the crisis.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, capital_mobility_operators, excluded,
    powerful, biographical, mobile, global).

% Economists, policymakers, and government officials committed to demand-management fiscal policy as the primary tool of macroeconomic stabilization. The gold standard constrains fiscal capacity; its abandonment vindicates their policy framework by removing the monetary constraint. They benefit directly from the collapse because it permits unrestricted fiscal stimulus—war spending, social programs, and counter-cyclical stimulus all become feasible without gold discipline.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policy_establishment, beneficiary,
    institutional, biographical, arbitrage, national).

% The structural position the US occupies as issuer of the global reserve currency. This role is both a privilege (seigniorage from dollar use) and a constraint (discipline from gold backing). The transition from gold-backed to fiat dollar reflects a renegotiation of that role—the US preserves the privilege, surrenders the discipline. Other analysts note this role cannot be delegated; whoever issues the reserve currency faces the same Triffin-like tension.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, international_hegemon_role, observer,
    institutional, civilizational, analytical, global).

% The telecommunications and banking infrastructure that enables near-instantaneous capital movement across borders. In 1944–1960, this infrastructure was primitive; by 1968–1971 it had matured. The constraint's persistence depends partly on technological friction in moving capital; as friction falls, both the ability and the incentive to exit the system increase. This is not an actor but a structural condition that amplifies the other pressures.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, technological_capital_mobility_infrastructure, excluded,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(monetary_anchor_principle__overdetermined_composite_reading, technological_capital_mobility_infrastructure).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monetary_anchor_principle__overdetermined_composite_reading, us_fiscal_administration).
narrative_ontology:fixing_cost_class(monetary_anchor_principle__overdetermined_composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The gold standard provided a common anchor for international monetary values: fixed parities between currencies meant stable exchange rates for trade, predictable relative prices for international contracts, and a shared nominal constraint that disciplined all signatories equally. It solved the problem of how to denominate cross-border transactions and manage exchange-rate volatility in a world of rising trade.
% TRANSFER_FUNCTION: Moves the burden of monetary discipline from the reserve-currency issuer to other central banks and ultimately to the real economy via inflation risk. US fiscal deficits are monetized through dollar creation; foreign holders absorb the inflation externality because dollars are held as reserves. The constraint transfers seigniorage privilege to the US (it can run deficits without immediate gold loss) but also transfers inflation risk to its trading partners.
% ABSENT_VOICES: Wage workers and savers in debtor countries (especially US allies like the UK and France) who bore inflation costs as the dollar was progressively devalued. Creditor-country electorates in Germany and Japan, who questioned why they were financing US deficits. Developing economies outside the Bretton Woods inner circle, who had no seat in the institutions but faced currency instability and commodity-price shocks as the transition occurred. These groups would object to bearing the transition's costs but were kept out of the negotiation process entirely.
% DISAPPEARANCE_RATIONALE: If the overdetermined pressures had NOT materialized (e.g., if the Triffin dilemma had been solved by multilateral reserve diversification, or if Vietnam War deficits had been funded differently, or if Keynesian policy had remained unpopular), the gold standard might have persisted into the 1980s or been replaced by a different mechanism than floating fiat. The constraint's disappearance would rearrange: fixed exchange rates would hold longer, capital controls might remain effective, inflation would be more tightly disciplined. Instead, the multiple pressures converged and forced an outcome—showing that the constraint was not natural but contingent on a specific policy and capital configuration.
% FOUNDING_PROBLEM: After World War II, the global economy needed a monetary standard acceptable to all trading nations, with fixed exchange rates to enable commerce and a common store of value. Gold was chosen as the numeraire; the US offered to exchange gold for dollars at $35/oz to anchor the system. This solved the immediate postwar coordination problem: trade partners had a reason to hold dollars (convertibility into gold) and a predictable exchange rate.
% FOUNDING_PROBLEM_CORROBORATION: The founding coordination problem—enabling stable trade and a shared nominal anchor—was solved successfully by 1950. By 1968, independent analysts and officials from multiple countries (Triffin himself, Robert Mundell, German Finance Ministry economists, eventually US Federal Reserve chairs like Paul Volcker) attested that the original problem no longer drove the system's persistence. The system persisted because it distributed benefits (US seigniorage, European stability, Japanese export advantage) that interested parties were reluctant to surrender—not because the founding problem remained live. The official US acknowledgment came in August 1971: the founding problem of postwar reconstruction was complete; the constraint's continued enforcement had become extraction.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__overdetermined_composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__overdetermined_composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monetary_anchor_principle__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__overdetermined_composite_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises monotonically from 0.38 (1960, when founding coordination problem was still recent) to 0.82 (1971, when extraction was obvious and coordination ceased). Suppression requirement rises steeply 1960–1968 (London Gold Pool formed 1961, active defense against runs) then plateaus 1968–1971 as suppression becomes insufficient—the system breaks despite intensive enforcement. Theater ratio stays low (0.08–0.28) because the coordination function was real throughout: exchange-rate stability and trade enabling were genuine until collapse. The low theater indicates extraction was the binding issue, not performative overhead. The measurement series is authored on a single shared grid (1960, 1963, 1966, 1968, 1970, 1971) so every metric has a value at every time point—no misalignment.
 *
 * PERSPECTIVAL GAP:
 *   From the US fiscal administration's seat, the constraint was increasingly burdensome extraction masquerading as coordination—the real gains from stability accrued to trade partners, while the US bore the cost of defending the peg. From foreign central banks' seats, the constraint was initially genuine coordination (predictable rates, dollar stability) that gradually became a mechanism for exporting US inflation—they were forced to absorb the real loss. From the Keynesian establishment's seat, the constraint was an outdated relic blocking rational policy. The engine computes these divergences from the structural data: beneficiary/victim declarations, exit options, power asymmetry. The analysis does not reconcile them; it flags them as seats that should compute to different type classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   US fiscal administration: beneficiary (benefits from constraint abandonment, collects seigniorage), powerful, arbitrage exit (can unilaterally change rules), d ≈ 0.15 (strong beneficiary). Foreign central banks: payers (absorb inflation and redemption timing pressure), institutional power, constrained exit (cannot easily diversify reserves or abandon dollar dependence), d ≈ 0.85 (targets). Monetary discipline regime: abstract victim, d approaches 1.0 (pure extraction target—the constraint extracts compliance from this principle). Capital mobility operators: analytically excluded from policy process but structurally driving breakdown through exit; their d is mixed (they benefit from constraint abandonment via lower capital controls, but are formally excluded from the bargain). The Keynesian establishment: beneficiaries (doctrine vindicated by constraint abandonment), institutional power, arbitrage exit (can switch to fiat policy), d ≈ 0.20 (beneficiaries).
 *
 * MANDATROPHY ANALYSIS:
 *   The overdetermined composite reading resolves the mandatrophy question by showing that the constraint's founding problem (postwar coordination, fixed exchange rates enabling trade) was genuine and served its purpose. By 1965–1968, the founding problem was dead—trade was stable, the postwar economy was rebuilt, and the constraint persisted only to preserve US fiscal privilege. The tangled-rope classification prevents misidentifying this as either pure rope (it retained real coordination function for trade) or pure snare (it genuinely did solve an initial coordination problem). The measurement trajectory (extractiveness rising as founding problem died) tracks the mandatrophy transition: coordination function persisting inertially while extraction became the binding factor. By 1971, both extraction and suppression had peaked and the system was unable to persist—the constraint broke because it had shifted from solving a live problem to preserving a specific distribution of gains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    overdetermination_counterfactual,
    'If ONE of the four pressures (Triffin, Vietnam deficits, Keynesian consensus, capital mobility) had been absent or reversed, would the gold standard have persisted into the 1980s or transitioned differently?',
    'Comparative historical analysis of similar monetary regimes under partial pressure (UK pound under Suez Crisis without Vietnam-scale fiscal burden; or thought-experiment modeling of 1971 without capital-mobility infrastructure). Central banks'' own contingency planning documents from the 1960s-1980s period.',
    'If the answer is ''yes, one absent pressure would have changed the outcome,'' then the overdetermination reading is supported and the constraint type is confirmed as tangled_rope (multiple causal pathways, not a single bottleneck). If ''no, any one of the four alone was sufficient,'' then the constraint type might downgrade to a snare driven by a single extractive logic, or the causal story needs revision.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(overdetermination_counterfactual, conceptual, 'Whether overdetermination is real or constructed for narrative coherence.').

omega_variable(
    policy_choice_vs_structural_inevitability,
    'How much of the 1971 collapse was the result of structural pressures making any other outcome implausible (the overdetermination reading''s claim) versus deliberate policy choice by the Nixon administration to escape fiscal constraints (the punctuated_swap reading''s claim)?',
    'Analysis of declassified National Security Council minutes, Federal Reserve records, and international monetary negotiations 1968–1971. Interviews with surviving policymakers. Comparison of contingency scenarios the administration actually discussed versus the structural pressures they faced.',
    'If policymakers had genuine alternatives and chose abandonment for ideological or strategic reasons, the constraint type shifts toward snare (extraction via coercion and foreclosure of alternatives). If they faced genuine structural pressure that left abandonment as the only viable option, the tangled_rope classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_choice_vs_structural_inevitability, empirical, 'Structural determination versus human agency in the transition.').

omega_variable(
    capital_mobility_infrastructure_causality,
    'Was technological capital mobility a proximate cause of the 1971 breakdown (facilitating capital flight that accelerated crisis), or a background condition that became salient only when other pressures created incentive to flee?',
    'Time-series analysis of capital flows 1960–1971, isolated from other variables. Study of earlier monetary crises (1931 bank runs, 1960s sterling crisis) with different technological infrastructure to see whether capital mobility alone can trigger breakdown. Analysis of capital controls'' effectiveness in slowing the 1971 crisis.',
    'If capital mobility was proximate-causal, the constraint''s persistence depended heavily on suppressing capital flight—supporting the high suppression_requirement readings and the characterization of active enforcement. If capital mobility was only enabling, suppression of actual capital might have been less intensive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_mobility_infrastructure_causality, empirical, 'Role of technological infrastructure in constraint breakdown.').

omega_variable(
    reading_foreclosure_test,
    'Does the overdetermined_composite_reading logically foreclose the triffin_inevitability_reading, or do they coexist as compatible framings?',
    'Structural analysis of causal necessity vs. sufficiency relationships claimed by each reading.',
    'If the readings foreclose each other, this omega should report ''forecloses'' in the reading_relations block and both readings cannot be simultaneously true. If they coexist, report ''coexists_with.'' This resolves the relation type for the cs_structure block.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Logical relationship between the overdetermined_composite_reading and sibling readings in the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__overdetermined_composite_reading, 1960, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1960, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1960, 0.08).
narrative_ontology:measurement(mone_tr_t1963, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1963, 0.12).
narrative_ontology:measurement(mone_tr_t1966, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1966, 0.18).
narrative_ontology:measurement(mone_tr_t1968, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1968, 0.23).
narrative_ontology:measurement(mone_tr_t1970, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1970, 0.27).
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1971, 0.28).

% Extraction over time
narrative_ontology:measurement(mone_be_t1960, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1960, 0.38).
narrative_ontology:measurement(mone_be_t1963, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1963, 0.48).
narrative_ontology:measurement(mone_be_t1966, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1966, 0.62).
narrative_ontology:measurement(mone_be_t1968, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1968, 0.71).
narrative_ontology:measurement(mone_be_t1970, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1970, 0.78).
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1971, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t1960, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1960, 0.35).
narrative_ontology:measurement(mone_su_t1963, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1963, 0.42).
narrative_ontology:measurement(mone_su_t1966, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1966, 0.54).
narrative_ontology:measurement(mone_su_t1968, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1968, 0.63).
narrative_ontology:measurement(mone_su_t1970, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1970, 0.68).
narrative_ontology:measurement(mone_su_t1971, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1971, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__overdetermined_composite_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(monetary_anchor_principle__overdetermined_composite_reading, 0.12).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, triffin_dilemma_as_mountain).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, vietnam_war_fiscal_deficits).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, keynesian_demand_management).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, capital_mobility_constraint__1960s).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle__punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle__triffin_inevitability_reading).

% DUAL FORMULATION NOTE:
% The monetary_anchor_principle kernel has three readings: (1) overdetermined_composite_reading emphasizes multiple convergent structural pressures (this story); (2) punctuated_swap_reading emphasizes discrete policy choice on August 15, 1971; (3) triffin_inevitability_reading emphasizes the Triffin dilemma as the primary driver. These are three different constraint stories sharing one kernel—the Bretton Woods gold standard commitment. Each has its own epsilon, its own beneficiary/victim structure, and its own classification. The readings coexist as live positions held by different analysts and policymakers; none logically forecloses the others (though they make different empirical claims about causality).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monetary_anchor_principle__overdetermined_composite_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
