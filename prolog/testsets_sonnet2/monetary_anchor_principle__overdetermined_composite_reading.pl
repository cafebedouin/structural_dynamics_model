% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Post-Bretton Woods Fiat Discretion as Overdetermined Structural Outcome
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   The overdetermined-composite reading holds that by the late 1960s, four
 *   independent but mutually reinforcing structural pressures — the Triffin
 *   dilemma's liquidity-versus-confidence tension, Vietnam War deficit
 *   spending straining the balance of payments, an ascendant Keynesian
 *   consensus normalizing discretionary fiscal policy, and
 *   technologically-driven capital mobility (Eurodollar markets) that made
 *   capital controls porous — jointly made the gold-dollar link's collapse
 *   effectively inevitable. This reading treats the transition as
 *   multiply-caused necessity rather than a discrete choice (contra
 *   punctuated_swap_reading) or a single sufficient cause (contra
 *   triffin_inevitability_reading). Because the reading treats the outcome as
 *   structurally overdetermined, it functions to diffuse responsibility
 *   across impersonal forces even though each stream (deficit spending,
 *   policy consensus, capital account liberalization) was itself the product
 *   of identifiable actors' decisions.
 *
 * KEY AGENTS:
 *   - us_federal_fiscal_authority: primary beneficiary (institutional/arbitrage) — gains discretionary fiscal capacity
 *   - keynesian_policy_establishment: beneficiary (institutional/mobile) — theoretical framework vindicated, institutional mandate expanded
 *   - fixed_income_savers: primary victim (powerless/trapped) — bears inflation costs with no exit
 *   - foreign_dollar_reserve_holders: secondary victim (institutional/constrained) — absorbs unilateral revaluation loss
 *   - economic_historians: analytical observer — adjudicates the kernel contest this story is one reading of
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, 0.78).
domain_priors:suppression_score(monetary_anchor_principle__overdetermined_composite_reading, 0.6).
domain_priors:theater_ratio(monetary_anchor_principle__overdetermined_composite_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(monetary_anchor_principle__overdetermined_composite_reading, "Post-Bretton Woods Fiat Discretion as Overdetermined Structural Outcome").
narrative_ontology:topic_domain(monetary_anchor_principle__overdetermined_composite_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__overdetermined_composite_reading, '03e29d0d-2d81-4086-b761-c59d8562d8c8').
narrative_ontology:cs_kernel_codification('03e29d0d-2d81-4086-b761-c59d8562d8c8', distributed).
narrative_ontology:cs_authority_grounding('03e29d0d-2d81-4086-b761-c59d8562d8c8', distributed).
narrative_ontology:cs_reading_relation('03e29d0d-2d81-4086-b761-c59d8562d8c8', monetary_anchor_principle__punctuated_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('03e29d0d-2d81-4086-b761-c59d8562d8c8', monetary_anchor_principle__triffin_inevitability_reading, influences).
narrative_ontology:cs_axiom('03e29d0d-2d81-4086-b761-c59d8562d8c8', foundational, causal_streams_are_jointly_necessary_and_mutually_reinforcing).
narrative_ontology:cs_axiom_status(causal_streams_are_jointly_necessary_and_mutually_reinforcing, holdable).
narrative_ontology:cs_axiom_grounding('03e29d0d-2d81-4086-b761-c59d8562d8c8', causal_streams_are_jointly_necessary_and_mutually_reinforcing, empirically_contingent).
narrative_ontology:cs_axiom('03e29d0d-2d81-4086-b761-c59d8562d8c8', secondary, no_single_stream_was_independently_sufficient).
narrative_ontology:cs_axiom_status(no_single_stream_was_independently_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('03e29d0d-2d81-4086-b761-c59d8562d8c8', no_single_stream_was_independently_sufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('03e29d0d-2d81-4086-b761-c59d8562d8c8', bretton_woods_convertibility_regime).
narrative_ontology:cs_drift_state('03e29d0d-2d81-4086-b761-c59d8562d8c8', post_1971_discretionary_fiat_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('03e29d0d-2d81-4086-b761-c59d8562d8c8', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, us_federal_fiscal_authority).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policy_establishment).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, multinational_capital_arbitrageurs).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, fixed_income_savers).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, foreign_dollar_reserve_holders).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, future_taxpayers_bearing_inflation_costs).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__overdetermined_composite_reading, structural_overdetermination_thesis).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__overdetermined_composite_reading, triffin_dilemma_formalization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs persistent deficits (Vietnam War spending, Great Society programs) that would have been constrained by gold convertibility. Once the anchor is removed, gains discretionary fiscal and monetary capacity unavailable under the prior regime. Administers the closing of the gold window and subsequent floating-rate architecture.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, us_federal_fiscal_authority, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__overdetermined_composite_reading, us_federal_fiscal_authority, agenda_setter).

% Academic and central-bank consensus favoring demand management over rigid metallic constraint. Benefits professionally and institutionally from a policy environment where discretionary countercyclical action is legitimate; the anchor's removal validates their theoretical framework and expands their institutional mandate.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policy_establishment, beneficiary,
    institutional, generational, mobile, national).

% Eurodollar market participants and multinational corporations exploiting the technological increase in capital mobility that made the fixed-rate system increasingly unenforceable. Profit from currency arbitrage and cross-border capital flows both before and after the transition; largely indifferent to the specific outcome as long as mobility persists.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, multinational_capital_arbitrageurs, beneficiary,
    organized, biographical, arbitrage, global).

% Hold savings, pensions, and fixed-rate instruments whose real value erodes once the inflation constraint imposed by gold convertibility is removed. Have no meaningful exit — cannot easily reallocate into inflation-hedged assets, lack the sophistication or capital mobility available to arbitrageurs.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, fixed_income_savers, payer,
    powerless, biographical, trapped, national).

% Foreign central banks (France, West Germany, Japan) that accumulated dollar reserves under the Bretton Woods convertibility promise. When convertibility is suspended, they absorb a unilateral revaluation loss and are left holding a fiat instrument they did not choose; their exit is constrained by the systemic role of the dollar in trade settlement, which makes wholesale abandonment self-damaging.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, foreign_dollar_reserve_holders, payer,
    institutional, generational, constrained, global).

% Inherit the inflationary consequences (1970s stagflation) of the removed discipline mechanism without having participated in the decision. Cannot exit the currency regime they are born into; bear diffuse costs through eroded purchasing power and subsequent disinflationary recessions imposed to correct the excess.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, future_taxpayers_bearing_inflation_costs, payer,
    powerless, generational, trapped, national).

% Hard-money economists and a minority of policymakers who argued the deficits and Triffin pressure were themselves policy choices, not structural inevitabilities, and that fiscal restraint could have preserved convertibility. Their position was marginalized in the Keynesian-dominated policy discourse of the era and is largely absent from the historical narrative this reading privileges.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, gold_standard_defenders, excluded,
    moderate, civilizational, trapped, national).

% Retrospectively assess whether the 1971 transition was structurally overdetermined or a series of avoidable discrete choices. Their disagreement constitutes the kernel contest this story is one reading of.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monetary_anchor_principle__overdetermined_composite_reading, us_federal_fiscal_authority).
narrative_ontology:fixing_cost_class(monetary_anchor_principle__overdetermined_composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Removing the gold anchor allowed monetary and fiscal authorities to coordinate demand management across a complex, multi-causal environment (war financing, capital mobility, reserve currency obligations) that a rigid convertibility rule could not simultaneously satisfy — it resolved a genuine multi-constraint optimization problem the fixed system could not solve.
% TRANSFER_FUNCTION: Moves purchasing power and monetary discipline costs from state fiscal actors (who gain discretionary capacity) to holders of dollar-denominated fixed claims — domestic savers, foreign central banks, and future taxpayers who inherit inflation and stabilization costs.
% ABSENT_VOICES: Hard-money economists and the countries most exposed as reserve holders (particularly France under de Gaulle, who explicitly warned of this outcome) argued the deficits were discretionary policy failures, not structural necessities; this reading's framing of 'overdetermination' backgrounds the extent to which each causal stream (Vietnam spending, Great Society programs) was itself a contingent political choice rather than an exogenous force.
% DISAPPEARANCE_RATIONALE: If the overdetermined-composite reading were rejected in favor of a contingent-choice account, the moral and causal responsibility for the transition would shift from 'structural forces' to identifiable policy actors (Nixon administration, Fed, Congress), which would rearrange accountability narratives even though the monetary regime itself would not un-happen; parties dispute whether the reading changes anything material or only changes blame allocation.
% FOUNDING_PROBLEM: The reading was constructed to explain why the Bretton Woods gold-dollar link collapsed by 1971: it asserts the collapse was the joint, mutually-reinforcing product of the Triffin dilemma, Vietnam-era deficits, the Keynesian policy consensus normalizing deficit spending, and technologically-enabled capital mobility that made fixed rates unenforceable — no single stream was sufficient alone, but their conjunction made collapse effectively unavoidable by the late 1960s.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians outside the direct beneficiary set (academic monetary historians, IMF retrospective analyses) broadly corroborate that multiple pressures co-occurred, but contest whether their combination was truly overdetermining or whether counterfactual fiscal restraint in any one stream (particularly Vietnam spending) could have preserved convertibility into the 1970s — the corroboration is partial and the inevitability claim itself remains a live historiographical dispute, not a settled external verdict.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__overdetermined_composite_reading, contested).
narrative_ontology:founding_problem_status(monetary_anchor_principle__overdetermined_composite_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__overdetermined_composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monetary_anchor_principle__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__overdetermined_composite_reading, 0.78, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises from 0.35 (1958, gold-dollar link still broadly credible) to 0.78 by 1974 (post-suspension, inflation fully realized) — modeling the reading's own claim that the composite pressures accumulated rather than emerging suddenly. Suppression is moderate-high (0.6) because maintaining the post-1971 discretionary regime requires continuous institutional defense (capital account management, reserve currency diplomacy) rather than passive persistence. Theater ratio rises to 0.42 reflecting the increasing role of technical justification (SDRs, managed floating rhetoric) papering over the underlying discretionary reality once the anchor was gone.
 *
 * PERSPECTIVAL GAP:
 *   From the fiscal-authority seat, the transition reads as a rope — a necessary adaptation to structural pressures that no single actor could have prevented. From the fixed-income-saver or foreign-reserve-holder seat, the same event reads as an imposed transfer: a discipline mechanism was removed by parties who benefited from its removal, and the costs were displaced onto people with no seat in the decision. The overdetermination framing itself is part of what produces this gap — treating the outcome as structurally inevitable makes the resulting extraction harder to contest than it would be under a discrete-choice framing.
 *
 * DIRECTIONALITY LOGIC:
 *   US fiscal authority and the Keynesian establishment sit near the full-beneficiary end: they gained capacity and were largely insulated from the costs (low d, arbitrage/mobile exit). Fixed-income savers and future taxpayers sit near the full-target end: trapped exit, powerless, and the costs (inflation, purchasing-power erosion) are diffuse but severe and not consented to. Foreign reserve holders occupy an intermediate but still victim-leaning position: institutional power gives them some voice, but their exit is constrained by systemic entanglement with the dollar, which the arrangement itself created.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'overdetermination' claim risks functioning as a mandatrophy-obscuring device: by attributing the transition to impersonal structural forces (Triffin dynamics, capital mobility, war financing) rather than to specific, reversible policy choices (funding Vietnam through deficits rather than taxation; the Fed's accommodation of it), the reading forecloses the question of whether the underlying founding problem — needing monetary flexibility to manage a genuine crisis — remains live decades later, or whether the flexibility became a standing arrangement whose original justification (crisis management) no longer applies. Classifying this as tangled_rope rather than mountain keeps that question open rather than letting the overdetermination language pre-empt it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    overdetermination_vs_contingent_conjunction,
    'Were the four causal streams (Triffin dilemma, Vietnam deficits, Keynesian consensus, capital mobility) truly independent and jointly necessary, or were several of them themselves downstream of a single set of contingent political choices (e.g., the decision to finance Vietnam via deficit rather than taxation, which fed both the Triffin pressure and the fiscal stream)?',
    'Counterfactual historical modeling: if Vietnam War spending had been tax-financed rather than deficit-financed, would the Triffin dilemma and capital mobility pressures alone have been sufficient to force gold-window closure by 1971? Requires comparative analysis against other reserve-currency episodes.',
    'If the streams are shown to be substantially non-independent (several downstream of the same political choice), the overdetermination claim weakens toward a more contingent, actor-driven account — shifting this reading toward greater compatibility with punctuated_swap_reading and reducing the apparent inevitability that currently supports treating removed monetary discipline as a structural rather than chosen outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overdetermination_vs_contingent_conjunction, empirical, 'Whether the four causal streams are genuinely independent or share a common contingent root.').

omega_variable(
    beneficiary_narrative_selection,
    'Is the overdetermined-composite framing itself favored by historians and policymakers partly because it diffuses accountability away from the specific institutional actors (Fed, Treasury, Nixon administration) who benefited from the discretionary regime that followed?',
    'Discourse analysis of when and by whom the ''overdetermination'' framing was first advanced relative to the ''discrete choice'' framing, and whether its prevalence correlates with institutional interest in normalizing the outcome.',
    'If the framing''s prevalence tracks beneficiary interest rather than independent historiographical merit, this strengthens the case that the ''mountain-like'' language of inevitability is doing extractive cover work — supporting the tangled_rope classification over any harder mountain reading of this kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_narrative_selection, conceptual, 'Whether the overdetermination narrative''s popularity reflects genuine explanatory power or beneficiary-interest selection.').

omega_variable(
    reversibility_of_composite_causes,
    'Given that the reading treats the transition as caused by a composite of structural pressures, is the resulting monetary regime reversible only by addressing all causal streams simultaneously, or could addressing any single stream (e.g., restoring capital controls) substantially restore monetary discipline?',
    'Comparative study of countries that reintroduced partial capital controls or fiscal rules post-1971 and whether monetary discipline outcomes improved proportionally.',
    'High irreversibility (requiring all streams addressed) supports the high ε and tangled_rope framing as a durable, hard-to-dislodge arrangement; if any single stream''s reversal restores most discipline, the arrangement is less deeply entangled than claimed and easier to challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_of_composite_causes, empirical, 'Whether the composite causation implies composite irreversibility, as the reading''s high ε assumes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__overdetermined_composite_reading, 1958, 1976).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1958, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1958, 0.15).
narrative_ontology:measurement(mone_tr_t1963, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1963, 0.22).
narrative_ontology:measurement(mone_tr_t1968, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1968, 0.3).
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1971, 0.4).
narrative_ontology:measurement(mone_tr_t1974, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1974, 0.42).
narrative_ontology:measurement(mone_tr_t1976, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1976, 0.42).

% Extraction over time
narrative_ontology:measurement(mone_be_t1958, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1958, 0.35).
narrative_ontology:measurement(mone_be_t1963, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1963, 0.48).
narrative_ontology:measurement(mone_be_t1968, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1968, 0.6).
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1971, 0.72).
narrative_ontology:measurement(mone_be_t1974, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1974, 0.78).
narrative_ontology:measurement(mone_be_t1976, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1976, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t1958, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1958, 0.3).
narrative_ontology:measurement(mone_su_t1963, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1963, 0.38).
narrative_ontology:measurement(mone_su_t1968, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1968, 0.48).
narrative_ontology:measurement(mone_su_t1971, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1971, 0.58).
narrative_ontology:measurement(mone_su_t1974, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1974, 0.6).
narrative_ontology:measurement(mone_su_t1976, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1976, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__overdetermined_composite_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(monetary_anchor_principle__overdetermined_composite_reading, 0.12).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, triffin_inevitability_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the monetary_anchor_principle kernel. punctuated_swap_reading treats the 1971 closure as a discrete institutional decision abstracted from causal buildup (lower structural ε, more contingent/reversible framing, potentially rope-leaning if framed as pure coordination adjustment). triffin_inevitability_reading isolates the Triffin dilemma alone as independently sufficient (a narrower mountain-leaning claim about reserve-currency mechanics). This reading (overdetermined_composite_reading) claims all four streams were jointly necessary and treats the resulting regime as a tangled_rope: real coordination function (adaptive flexibility) entangled with asymmetric extraction (fiscal beneficiaries vs. fixed-claim holders). The three readings share a referent (the standing post-1971 discretionary monetary arrangement) but author different ε, different beneficiary/victim structures, and different types — consistent with the ε-invariance decomposition principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
