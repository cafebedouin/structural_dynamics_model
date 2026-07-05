% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__triffin_structural_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [RESOLVED MANDATROPHY]
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Bretton Woods Dollar-Gold Convertibility as Structurally Unsustainable Design (Triffin Reading)
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   This story instantiates the Triffin structural reading of the dollar-gold
 *   convertibility kernel: convertibility was not a violated legal promise
 *   (the strict_convertibility_reading) nor a conditional policy tool
 *   subordinated to domestic priorities (the policy_flexible_reading), but a
 *   design that was mathematically incapable of persisting regardless of
 *   good-faith administration. As US dollar liabilities held abroad grew to
 *   service global liquidity needs, the ratio of gold reserves to dollar
 *   claims necessarily deteriorated -- the 'Triffin dilemma.' No policy
 *   choice by US administrators or creditor central banks could resolve the
 *   underlying incompatibility between reserve-currency issuance and fixed
 *   convertibility; the 1971 Nixon Shock is read here as the structurally
 *   overdetermined resolution of an impossible trilemma, not a discretionary
 *   abandonment of an obligation that could otherwise have been honored. Both
 *   the US (structurally, as issuer trapped between liquidity provision and
 *   convertibility credibility) and creditor nations (structurally, as
 *   reserve holders whose claims outran gold backing) are victims of the same
 *   design flaw; the beneficiaries are those who designed and administer the
 *   post-1971 floating-rate regime that the collapse made necessary and
 *   legitimate.
 *
 * KEY AGENTS:
 *   - us_monetary_policymakers: trapped by incompatible mandates (institutional/trapped)
 *   - creditor_nation_central_banks: structurally exposed reserve holders (institutional/constrained)
 *   - developing_country_reserve_holders: powerless bystanders to the collapse (powerless/trapped)
 *   - us_treasury_short_term: transient beneficiary of exorbitant privilege (institutional/arbitrage)
 *   - post_bretton_woods_floating_regime_architects: beneficiaries of the vindicated diagnosis (institutional/analytical)
 *   - robert_triffin_and_structural_economists: analytical observers who named the mechanism (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, 0.71).
domain_priors:suppression_score(dollar_gold_convertibility__triffin_structural_reading, 0.58).
domain_priors:theater_ratio(dollar_gold_convertibility__triffin_structural_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__triffin_structural_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__triffin_structural_reading, "Bretton Woods Dollar-Gold Convertibility as Structurally Unsustainable Design (Triffin Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__triffin_structural_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__triffin_structural_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__triffin_structural_reading, '338e26f6-174e-489d-8ef2-d24379f67c8a').
narrative_ontology:cs_kernel_codification('338e26f6-174e-489d-8ef2-d24379f67c8a', formalized).
narrative_ontology:cs_authority_grounding('338e26f6-174e-489d-8ef2-d24379f67c8a', extraction).
narrative_ontology:cs_interpretation_layer_present('338e26f6-174e-489d-8ef2-d24379f67c8a').
narrative_ontology:cs_reading_relation('338e26f6-174e-489d-8ef2-d24379f67c8a', dollar_gold_convertibility__strict_convertibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('338e26f6-174e-489d-8ef2-d24379f67c8a', dollar_gold_convertibility__policy_flexible_reading, forecloses).
narrative_ontology:cs_axiom('338e26f6-174e-489d-8ef2-d24379f67c8a', foundational, convertibility_design_mathematically_unsustainable).
narrative_ontology:cs_axiom_status(convertibility_design_mathematically_unsustainable, holdable).
narrative_ontology:cs_axiom_grounding('338e26f6-174e-489d-8ef2-d24379f67c8a', convertibility_design_mathematically_unsustainable, empirically_contingent).
narrative_ontology:cs_axiom('338e26f6-174e-489d-8ef2-d24379f67c8a', foundational, collapse_is_structural_not_discretionary).
narrative_ontology:cs_axiom_status(collapse_is_structural_not_discretionary, holdable).
narrative_ontology:cs_axiom_grounding('338e26f6-174e-489d-8ef2-d24379f67c8a', collapse_is_structural_not_discretionary, empirically_contingent).
narrative_ontology:cs_reference_frame('338e26f6-174e-489d-8ef2-d24379f67c8a', bretton_woods_fixed_parity_system).
narrative_ontology:cs_drift_state('338e26f6-174e-489d-8ef2-d24379f67c8a', post_1971_nixon_shock, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('338e26f6-174e-489d-8ef2-d24379f67c8a', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime_architects).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, us_treasury_short_term).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, us_monetary_policymakers).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, creditor_nation_central_banks).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, developing_country_reserve_holders).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__triffin_structural_reading, triffin_dilemma_thesis).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__triffin_structural_reading, impossible_trinity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Must simultaneously supply the world's reserve asset by running persistent balance-of-payments deficits AND maintain gold convertibility at $35/oz to preserve confidence in the dollar. These two obligations are structurally incompatible over any long horizon: supplying enough dollars for global liquidity erodes the gold-cover ratio, while restricting dollar outflows to protect convertibility starves the world of reserves. There is no policy setting that satisfies both; every choice accelerates one failure mode or the other.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, us_monetary_policymakers, payer,
    institutional, generational, trapped, global).

% Accumulate dollar reserves earned through trade surpluses because the system requires someone to hold the reserve currency, but each dollar held is a claim on a gold stock that mathematically cannot back all outstanding claims. They face a prisoner's dilemma: converting dollars to gold en masse would collapse the system they depend on for trade settlement, but not converting means absorbing a currency whose backing is eroding in real time. Their exit (mass conversion) is individually rational but collectively catastrophic, so it is exercised only partially and reluctantly (e.g. France under de Gaulle) until confidence finally breaks in 1971.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, creditor_nation_central_banks, payer,
    institutional, generational, constrained, global).

% Hold dollar reserves for trade and stability purposes with no capacity to influence US monetary policy or to credibly threaten conversion the way major creditor nations can. They bear the structural risk of dollar devaluation or system collapse without any seat at the table that designed the arrangement, and without the leverage to extract concessions during the period of mounting instability.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, developing_country_reserve_holders, payer,
    powerless, generational, trapped, global).

% Benefits in the short-to-medium run from the 'exorbitant privilege' of financing deficits by issuing a reserve currency the world must accept, effectively borrowing at below-market cost while the design flaw has not yet triggered collapse. This benefit is temporary and self-liquidating — it disappears once the Triffin dynamic forces the Nixon Shock in 1971, but during the interval it is a real transfer captured by the issuing state.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, us_treasury_short_term, beneficiary,
    institutional, immediate, arbitrage, national).

% The economists, central bankers, and policymakers (Volcker, Connally, and successors) who redesigned the international monetary order after 1971 are the structural beneficiaries of the convertibility system's demonstrated failure: the collapse vindicated the case for floating exchange rates and discretionary monetary policy unconstrained by a fixed gold anchor. Their institutional authority and the legitimacy of the subsequent regime rest on the diagnosed impossibility of the prior arrangement.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime_architects, beneficiary,
    institutional, civilizational, analytical, global).

% Economists (beginning with Triffin's 1959-60 testimony and writings) who identified the logical incompatibility between reserve-currency liquidity provision and convertibility credibility as a mathematical property of the system's design, independent of which officials administered it or how well-intentioned their policy choices were. Their analysis frames the collapse as structurally overdetermined rather than attributable to any single actor's mismanagement.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, robert_triffin_and_structural_economists, observer,
    analytical, civilizational, analytical, global).

% South Africa and other major gold producers had a material stake in how the convertibility peg was managed (the fixed $35/oz price versus market gold price) but were not parties to the Bretton Woods negotiations or the subsequent unwind decisions. Their interests in gold price policy were absent from the corridors where convertibility's fate was decided.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, gold_producing_nations, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime_architects).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__triffin_structural_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The gold-dollar peg was meant to solve a genuine coordination problem: providing a stable, universally accepted reserve asset for international trade and settlement after the collapse of the interwar gold standard and the chaos of competitive devaluations, without requiring every nation to hold physical gold.
% TRANSFER_FUNCTION: In the near term, the arrangement transferred seigniorage-like benefit to the United States (financing deficits via a currency others were obligated to accept) and transferred structural risk to creditor nations and developing reserve holders, who accumulated claims against a shrinking gold-backing ratio. At collapse, the transfer reversed sharply: unilateral suspension of convertibility (Nixon, 1971) transferred realized losses onto dollar holders who had trusted the peg.
% ABSENT_VOICES: Developing countries holding dollar reserves and gold-producing nations affected by the fixed gold price had no meaningful voice in the system's design or in the 1971 decision to suspend convertibility; they absorbed the consequences of a structural flaw diagnosed by economists and administered by great powers without their participation.
% DISAPPEARANCE_RATIONALE: The convertibility commitment's disappearance (which is exactly what happened in August 1971) did rearrange the world: it ended the Bretton Woods fixed-exchange-rate system entirely, launched the era of floating exchange rates, transformed the IMF's operational role, and restructured how nations manage reserves and monetary sovereignty to this day. This was not a minor administrative change but a foundational shift in the international monetary order.
% FOUNDING_PROBLEM: The founding problem was providing a credible, universally trusted international reserve asset and fixed exchange-rate anchor to enable stable postwar trade and reconstruction, after the demonstrated failures of both the classical gold standard's deflationary rigidity and the interwar floating-rate chaos.
% FOUNDING_PROBLEM_CORROBORATION: Robert Triffin's own testimony before Congress (1959-60) argued the design was headed toward structural failure before it occurred — corroboration from an analytical economist genuinely outside the beneficiary set of either the US Treasury or the creditor central banks. The subsequent adoption of floating rates by the entire international system after 1973, without serious attempts to restore gold convertibility, corroborates from outside any single party that the founding problem's proposed solution (fixed gold-dollar convertibility specifically) was judged unworkable by the community of nations that had to live with its replacement.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__triffin_structural_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__triffin_structural_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__triffin_structural_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dollar_gold_convertibility__triffin_structural_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__triffin_structural_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises steadily across the interval (0.22 to 0.71) because the gap between outstanding dollar liabilities and gold coverage widened continuously from 1944 through 1971 -- this is authored as a structural accumulation, not a policy choice at any single point. Theater ratio rises in parallel (0.15 to 0.62) because increasing amounts of diplomatic and institutional effort (the London Gold Pool, swap lines, special drawing rights negotiations) went into papering over the widening gap rather than resolving it, consistent with the Triffin reading's claim that no administrative fix could address a design-level incompatibility. Suppression (enforcement pressure to maintain confidence in the peg despite the underlying deterioration) rises correspondingly as the gap between claims and gold grew large enough to require active diplomatic and financial suppression of conversion demands.
 *
 * DIRECTIONALITY LOGIC:
 *   Both the US monetary authorities and creditor central banks are placed in the victim/payer set under this reading -- this is the reading's structural signature. The US did not choose extraction; it was structurally required to run deficits to supply world liquidity, which structurally undermined the convertibility promise it also had to defend. Creditor nations did not choose exposure; they were structurally required to hold dollar reserves for trade settlement, which structurally exposed them to a deteriorating gold-backing ratio they could not individually fix without triggering system-wide collapse. The beneficiary seat shifts to a later temporal position -- the architects of the succeeding floating-rate regime -- because the vindication of that regime's design logic is the actual asset the collapse produced.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a credible fixed international reserve anchor) is read as dead, not merely mismanaged: the Triffin reading holds that the arrangement's function could not be restored by better administration because the incompatibility was mathematical, not managerial. This is what distinguishes this reading from policy_flexible_reading (which would read the failure as a discretionary policy choice that could have gone otherwise) and from strict_convertibility_reading (which would read 1971 as a violated legal commitment rather than a structurally inevitable resolution). Status is marked RESOLVED MANDATROPHY because the post-1971 floating regime is read as the system's own correction of its design flaw.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_triffin,
    'Is the Bretton Woods collapse better explained as a structurally overdetermined design failure (this reading), a discretionary legal breach (strict_convertibility_reading), or a legitimate exercise of reserved policy flexibility (policy_flexible_reading)?',
    'The three readings are not adjudicated within this story; they are authored as separate constraints linked via network.affects_constraints. Resolution, if any, would require historical consensus on whether alternative policy paths (e.g. earlier gold price revaluation, tighter US fiscal discipline, SDR expansion) could have preserved convertibility indefinitely, which would weaken the structural-inevitability claim this reading makes.',
    'If historical counterfactual analysis established that convertibility could have been preserved through available policy tools, this reading''s foreclosure of policy_flexible_reading''s core premise would be undermined and the two readings would need to be re-typed as coexisting rather than one being more structurally fundamental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_triffin, conceptual, 'Which of the three kernel readings best accounts for the 1971 convertibility suspension.').

omega_variable(
    triffin_dilemma_mathematical_necessity,
    'Was the Triffin dilemma a strict mathematical necessity given the gold-dollar exchange rate and world trade growth rates, or a strong-but-contingent tendency that depended on specific US and creditor policy choices (e.g., persistent US deficits, France''s early conversion pressure)?',
    'Formal reconstruction of the gold-cover ratio trajectory under counterfactual policy scenarios (tighter US monetary policy, earlier gold revaluation, faster SDR adoption) using the historical trade and reserve growth data.',
    'If strictly necessary, this reading''s ''mountain-adjacent, inevitable design flaw'' framing is strongly supported and the tangled_rope classification (rather than snare) is justified by the genuine coordination function that existed before the flaw manifested. If contingent, some of the measured extraction should be attributed to specific policy choices rather than pure structural necessity, weakening the claim that the US and creditor nations were purely structural victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_dilemma_mathematical_necessity, empirical, 'Whether the Triffin dilemma was a strict logical necessity of the system''s parameters or a contingent outcome of specific policy paths.').

omega_variable(
    beneficiary_of_collapse_ambiguity,
    'Do the architects of the post-1971 floating-rate regime constitute a genuine beneficiary class, or is ''the floating regime'' better modeled as a vindicated proposition (a doctrine that gained credibility) rather than an actor that captured rents?',
    'Examine whether specific institutional actors (Federal Reserve leadership, IMF officials who redesigned Article IV in 1976, academic architects of monetarism and floating-rate theory) derived durable career, institutional-authority, or resource benefits from the regime change, versus the floating-rate doctrine being a diffuse intellectual outcome with no concentrated capturer.',
    'If no concentrated beneficiary exists, this constraint''s classification should be reconsidered toward piton or a scaffold-adjacent reading rather than tangled_rope, since tangled_rope requires an identifiable party who is coordinated-and-collects through the same structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_of_collapse_ambiguity, conceptual, 'Whether the post-1971 regime architects are a genuine capturing beneficiary class or a vindicated doctrine with no concentrated collector.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__triffin_structural_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1944, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1944, 0.15).
narrative_ontology:measurement(doll_tr_t1950, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(doll_tr_t1958, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1958, 0.32).
narrative_ontology:measurement(doll_tr_t1963, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1963, 0.44).
narrative_ontology:measurement(doll_tr_t1968, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1968, 0.55).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1971, 0.62).

% Extraction over time
narrative_ontology:measurement(doll_be_t1944, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1944, 0.22).
narrative_ontology:measurement(doll_be_t1950, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1950, 0.31).
narrative_ontology:measurement(doll_be_t1958, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1958, 0.45).
narrative_ontology:measurement(doll_be_t1963, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1963, 0.55).
narrative_ontology:measurement(doll_be_t1968, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1968, 0.66).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1971, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1944, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1944, 0.2).
narrative_ontology:measurement(doll_su_t1950, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1950, 0.25).
narrative_ontology:measurement(doll_su_t1958, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1958, 0.34).
narrative_ontology:measurement(doll_su_t1963, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1963, 0.42).
narrative_ontology:measurement(doll_su_t1968, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1968, 0.51).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1971, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__triffin_structural_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(dollar_gold_convertibility__triffin_structural_reading, 0.12).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility__strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility__policy_flexible_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, impossible_trinity_monetary_policy_autonomy).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, post_1971_floating_exchange_rate_regime).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the dollar_gold_convertibility kernel, all linked via affects_constraints. strict_convertibility_reading treats the 1971 suspension as breach of a binding Article IV legal obligation (US as sole obligated/breaching party). policy_flexible_reading treats convertibility as always conditional on domestic stability (1971 as legitimate policy exercise, no victims in the strong sense). This triffin_structural_reading treats both the US and creditor nations as structural victims of a mathematically incompatible design, with the beneficiary being the succeeding floating-rate regime rather than any party present at Bretton Woods. Each reading has a different ε trajectory, different beneficiary/victim sets, and different claimed_type; they must not be merged or averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
