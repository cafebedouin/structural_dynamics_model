% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Post-Bretton-Woods Fiat Discretion as Overdetermined Structural Necessity
 *   domain: monetary economics / political economy / international finance
 *
 * SUMMARY:
 *   This story instantiates the overdetermined-composite reading of the
 *   Bretton Woods collapse kernel: the claim that no single causal factor,
 *   and no single decision-maker, was responsible for the 1971 suspension of
 *   gold convertibility — rather, the Triffin dilemma, Vietnam-driven fiscal
 *   deficits, an entrenched Keynesian policy consensus favoring demand
 *   management over external discipline, and rising technological capital
 *   mobility converged to make collapse structurally inevitable by the late
 *   1960s regardless of any particular actor's choices. This is a
 *   tangled_rope: it genuinely coordinates fiscal and monetary policy space
 *   across multiple upstream pressures (a real coordination function), while
 *   also functioning as extraction — the removal of the gold anchor transfers
 *   monetary discipline away from savers and reserve-holding trading partners
 *   toward the fiscal and monetary authorities who gain discretion. The
 *   overdetermination framing itself does structural work: by presenting the
 *   outcome as inevitable given the causal streams, it forecloses scrutiny of
 *   which streams were themselves the product of discretionary choices (the
 *   decision to finance Vietnam and Great Society spending simultaneously
 *   without raising taxes, for instance) and therefore insulates the
 *   beneficiary seats from the argument that a different set of choices could
 *   have preserved some anchor.
 *
 * KEY AGENTS:
 *   - federal_fiscal_authorities: agenda_setter/beneficiary (institutional/arbitrage) — administers the transition and gains fiscal discretion
 *   - reserve_currency_issuer_state: beneficiary (institutional/arbitrage) — retains exorbitant privilege after the anchor is removed
 *   - keynesian_policy_establishment: beneficiary/agenda_setter (institutional/mobile) — theoretical framework vindicated by the composite account
 *   - fixed_income_savers: payer (powerless/trapped) — bear 1970s inflation cost
 *   - trading_partner_reserve_holders: payer (powerful/constrained) — absorb loss of convertibility promise
 *   - domestic_wage_earners: payer (powerless/trapped) — bear stagflation-era real wage erosion
 *   - monetary_historians: observer (analytical/analytical) — adjudicate among competing causal readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, 0.78).
domain_priors:suppression_score(monetary_anchor_principle__overdetermined_composite_reading, 0.62).
domain_priors:theater_ratio(monetary_anchor_principle__overdetermined_composite_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(monetary_anchor_principle__overdetermined_composite_reading, "Post-Bretton-Woods Fiat Discretion as Overdetermined Structural Necessity").
narrative_ontology:topic_domain(monetary_anchor_principle__overdetermined_composite_reading, "monetary economics / political economy / international finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__overdetermined_composite_reading, 'fd258bcf-e49d-4710-939d-1b5bf27bc4c8').
narrative_ontology:cs_kernel_codification('fd258bcf-e49d-4710-939d-1b5bf27bc4c8', distributed).
narrative_ontology:cs_authority_grounding('fd258bcf-e49d-4710-939d-1b5bf27bc4c8', distributed).
narrative_ontology:cs_reading_relation('fd258bcf-e49d-4710-939d-1b5bf27bc4c8', monetary_anchor_principle__punctuated_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('fd258bcf-e49d-4710-939d-1b5bf27bc4c8', monetary_anchor_principle__triffin_inevitability_reading, influences).
narrative_ontology:cs_axiom('fd258bcf-e49d-4710-939d-1b5bf27bc4c8', foundational, causal_overdetermination_of_regime_collapse).
narrative_ontology:cs_axiom_status(causal_overdetermination_of_regime_collapse, holdable).
narrative_ontology:cs_axiom_grounding('fd258bcf-e49d-4710-939d-1b5bf27bc4c8', causal_overdetermination_of_regime_collapse, empirically_contingent).
narrative_ontology:cs_axiom('fd258bcf-e49d-4710-939d-1b5bf27bc4c8', secondary, structural_convergence_dissolves_individual_agency_responsibility).
narrative_ontology:cs_axiom_status(structural_convergence_dissolves_individual_agency_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('fd258bcf-e49d-4710-939d-1b5bf27bc4c8', structural_convergence_dissolves_individual_agency_responsibility, conventional).
narrative_ontology:cs_reference_frame('fd258bcf-e49d-4710-939d-1b5bf27bc4c8', bretton_woods_gold_dollar_convertibility).
narrative_ontology:cs_drift_state('fd258bcf-e49d-4710-939d-1b5bf27bc4c8', nixon_shock_1971, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('fd258bcf-e49d-4710-939d-1b5bf27bc4c8', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, federal_fiscal_authorities).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, reserve_currency_issuer_state).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policy_establishment).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, fixed_income_savers).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, trading_partner_reserve_holders).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, domestic_wage_earners).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__overdetermined_composite_reading, structural_overdetermination_thesis).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__overdetermined_composite_reading, triffin_dilemma_diagnosis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the closing of the gold window and subsequent fiat discretion. Frames the closure as the only coherent response to a structurally overdetermined set of pressures — Vietnam deficits, Great Society spending, capital mobility, and Triffin exhaustion — rather than as a discretionary policy choice. Gains the capacity to run deficits and finance them through money creation without a gold constraint, converting what had been a hard external limit into a negotiable political one.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, federal_fiscal_authorities, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__overdetermined_composite_reading, federal_fiscal_authorities, beneficiary).

% As issuer of the world's reserve currency, retains exorbitant privilege — the ability to run persistent current account deficits financed in its own currency — after the anchor is removed. The overdetermined-composite framing insulates this outcome from scrutiny as a policy choice: if collapse was inevitable given the causal streams, no single actor bears responsibility for the resulting seigniorage advantage.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, reserve_currency_issuer_state, beneficiary,
    institutional, civilizational, arbitrage, global).

% Academic and central-banking consensus that had already built the theoretical case for demand management unconstrained by a metallic anchor. The composite-overdetermination reading vindicates this consensus retroactively: the causal streams it names (Triffin, war deficits, capital mobility) are precisely the pressures Keynesian analysis had long argued gold discipline could not accommodate. Their intellectual authority and policy discretion both expand once the anchor is gone.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policy_establishment, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policy_establishment, agenda_setter).

% Hold savings, pensions, and fixed-rate instruments denominated in the now-unanchored currency. Bear the cost of the inflation that fiat discretion enables through the 1970s. Cannot exit the currency system; the overdetermination framing forecloses the question of whether any of the causal streams could have been managed differently to preserve a nominal anchor, presenting the inflationary consequence as an unavoidable byproduct rather than a distributive choice.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, fixed_income_savers, payer,
    powerless, biographical, trapped, national).

% Foreign governments (notably France, under de Gaulle, and other gold-window redeemers) held dollar reserves under an implicit gold-convertibility promise. Absorb the loss when convertibility is suspended unilaterally. Constrained rather than trapped: they can diversify reserves over subsequent decades, but at the moment of suspension they have no recourse — the composite framing treats their claims as casualties of inevitable structural forces rather than of a decision made against their interests.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, trading_partner_reserve_holders, payer,
    powerful, generational, constrained, global).

% Experience the real-wage erosion of 1970s stagflation, which the overdetermined-composite reading attributes to the confluence of oil shocks, deficit spending, and the removed anchor acting together rather than to any single policy lever. Cannot exit the domestic labor market or currency regime; their bargaining position in the inflationary aftermath is treated as a downstream consequence of forces no one controlled.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, domestic_wage_earners, payer,
    powerless, biographical, trapped, national).

% Assess competing causal accounts of the 1971 transition — whether it was a discrete institutional decision, a Triffin-forced inevitability, or (this reading) an overdetermined composite where multiple independent causal streams converged such that removing any one would not have prevented collapse. Their scholarly disputes determine which reading dominates textbook and policy memory.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates fiscal and monetary policy discretion across a state facing simultaneously the Triffin liquidity-versus-confidence bind, war-driven deficits, an entrenched Keynesian demand-management consensus, and capital markets increasingly able to arbitrage fixed exchange rates — removing the gold anchor allows these pressures to be managed jointly rather than each independently exhausting a shared, non-renewable reserve constraint.
% TRANSFER_FUNCTION: Moves monetary discipline (the inflation constraint gold convertibility had imposed) away from currency holders, savers, and trading partners, and moves fiscal and monetary flexibility toward the issuing state and the policy establishment that manages it — savers and reserve-holding trading partners absorb the resulting inflation and currency-value erosion.
% ABSENT_VOICES: The counterfactual policymakers of 1968-1971 who might have pursued fiscal retrenchment, capital controls, or a renegotiated gold price to preserve some anchor are absent from the composite account by construction — the overdetermination framing treats their non-adoption as inevitable rather than examining why those paths were foreclosed by choices about Vietnam spending and domestic program commitments that were themselves discretionary.
% DISAPPEARANCE_RATIONALE: If the fiat-discretion regime this reading legitimizes were reversed and a hard anchor reimposed, fiscal authorities would lose deficit-financing flexibility and reserve-currency seigniorage would shrink — a real rearrangement for the beneficiary seats. But proponents of the composite reading argue the underlying pressures (capital mobility, reserve-currency liquidity demands) would simply reassert themselves against any new anchor, making the specific 1971 arrangement replaceable without changing the deeper structural condition — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: The Bretton Woods gold-dollar system faced a genuine coordination problem: providing enough dollar liquidity for growing world trade while maintaining gold convertibility credible enough to anchor confidence — a problem that, per this reading, several independent pressures (war deficits, Keynesian consensus, capital mobility) were simultaneously making unsolvable by conventional means.
% FOUNDING_PROBLEM_CORROBORATION: Central bank historians and IMF-affiliated economists who benefited from the resulting discretion corroborate the overdetermination account. Outside corroboration is thinner: monetarist critics (e.g., the Friedman-associated tradition) and some diplomatic historians examining the Nixon administration's internal deliberations argue the timing and manner of the August 1971 decision reflected discretionary political choices (protecting reelection prospects, avoiding devaluation stigma) layered on top of the structural pressures, not pure inevitability — so the corroboration for 'inevitable by late 1960s' specifically comes substantially from within the beneficiary tradition.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__overdetermined_composite_reading, contested).
narrative_ontology:founding_problem_status(monetary_anchor_principle__overdetermined_composite_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__overdetermined_composite_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extraction rises steadily across the interval (0.28 to 0.78) as the causal streams the composite reading names — Triffin pressure, war deficits, capital mobility, Keynesian consensus — accumulate and converge toward 1971, then stabilizes post-suspension as the new discretionary regime settles. Theater ratio is moderate (0.4 by 1976): a genuine coordination problem (liquidity provision for growing trade) existed, but a growing share of the justificatory apparatus around 'inevitability' serves to legitimize a discretionary outcome as though it were physically forced, which is itself a performative function. Suppression tracks the increasing political and institutional pressure applied to make continued gold convertibility untenable — capital controls, diplomatic pressure on gold-redeeming allies, and eventually unilateral suspension — before leveling off once the fiat regime is established and no longer needs active defense against a live alternative.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal fiscal authorities and the reserve-currency issuer state sit at the beneficiary end: they gain discretion and privilege from the anchor's removal, and the composite-overdetermination framing specifically serves their interest by making the outcome look causally forced rather than chosen. The Keynesian policy establishment benefits analytically and institutionally — their framework is vindicated. Fixed-income savers, trading-partner reserve holders, and domestic wage earners sit at the target end: they bear the transferred cost (inflation, currency devaluation, real wage erosion) and had no meaningful exit from the currency or trade relationships that exposed them to it. Trading partners get 'constrained' rather than 'trapped' because reserve diversification became possible over subsequent decades, but at the moment of suspension they had no recourse.
 *
 * MANDATROPHY ANALYSIS:
 *   The overdetermination reading resists a simple mandatrophy verdict precisely because its coordination function (liquidity provision under a growing multipolar trade system) may remain genuinely live even as the specific 1971 institutional resolution ages — this is why disappearance_verdict is 'contested' rather than a clean call. The framework prevents the mistake of treating either horn cleanly: it does not let the composite reading collapse into pure Mountain (unavoidable, no beneficiary) because identifiable beneficiaries (fiscal authorities, the issuing state) persist and gain discretion from the outcome; nor does it collapse into pure Snare, because a genuine liquidity-coordination problem existed independent of anyone's preference to exploit it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    overdetermination_vs_discretionary_choice,
    'Was the 1971 suspension of convertibility genuinely overdetermined by structural forces that made any single decision-maker''s choice immaterial to the outcome, or did discretionary political choices (financing Vietnam and domestic programs without raising taxes, timing the suspension around electoral considerations) constitute the actual proximate cause, with the ''structural inevitability'' framing serving retroactively to depoliticize a contested decision?',
    'Comparative institutional analysis: examine whether other reserve-adjacent economies facing similar Triffin-type and capital-mobility pressures in the same period pursued different paths (capital controls, fiscal retrenchment, gold price renegotiation) and achieved different outcomes, which would indicate the American outcome was not uniquely forced. Cross-reference declassified internal Nixon administration deliberations for evidence of discretionary weighing versus perceived necessity.',
    'If genuinely overdetermined, the tangled_rope''s beneficiary/victim structure reflects a structural transfer with no assignable political responsibility. If substantially discretionary, the ''overdetermination'' framing itself becomes part of the extraction mechanism — a legitimating narrative that shields the fiscal-authority beneficiary from accountability for a chosen distributive outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overdetermination_vs_discretionary_choice, conceptual, 'Whether structural overdetermination is a genuine causal finding or a legitimating retrofit for a discretionary decision.').

omega_variable(
    kernel_reading_selection_evidence,
    'What evidence or interpretive commitment leads an observer to select the overdetermined-composite reading over the punctuated_swap_reading (agency-centered) or the triffin_inevitability_reading (single-cause structural) of the same historical kernel?',
    'Track which reading dominates in which scholarly communities (international political economy vs. diplomatic history vs. monetarist economics) and whether the choice correlates with prior theoretical commitments (structuralist vs. agency-centered historiography) rather than with distinct evidence.',
    'If reading selection tracks prior theoretical commitment rather than new evidence, all three kernel readings should be treated as coexisting interpretive frameworks rather than as competing empirical hypotheses with a fact-of-the-matter resolution — reinforcing the coexists_with relation to both siblings rather than any foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, conceptual, 'Whether the choice among sibling kernel readings is evidence-driven or paradigm-driven.').

omega_variable(
    monetary_discipline_replaceability,
    'Could the inflation-constraint function gold convertibility served have been replaced by an alternative discipline mechanism (a strict monetary rule, an independent central bank mandate) without reproducing the fiscal-discretion extraction this reading identifies, or is discretionary fiat necessarily entangled with the loss of that discipline?',
    'Examine subsequent monetary regimes (post-1979 Volcker disinflation, inflation-targeting central banks from the 1990s onward) for evidence that discipline was substantially restored without reinstating a metallic anchor, which would indicate the victim-side cost (loss of monetary discipline) is not intrinsic to anchor removal but to the specific discretionary regime chosen in the 1970s.',
    'If discipline was substantially restorable through non-gold mechanisms, the victim classification (monetary discipline / fixed-income savers) narrows to the 1971-1979 window specifically rather than being a permanent structural feature of the post-Bretton-Woods order.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monetary_discipline_replaceability, empirical, 'Whether the lost monetary discipline was intrinsic to anchor removal or specific to the 1970s policy regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__overdetermined_composite_reading, 1958, 1976).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1958, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1958, 0.15).
narrative_ontology:measurement(mone_tr_t1963, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1963, 0.2).
narrative_ontology:measurement(mone_tr_t1968, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1968, 0.3).
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1971, 0.38).
narrative_ontology:measurement(mone_tr_t1973, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1973, 0.4).
narrative_ontology:measurement(mone_tr_t1976, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1976, 0.4).

% Extraction over time
narrative_ontology:measurement(mone_be_t1958, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1958, 0.28).
narrative_ontology:measurement(mone_be_t1963, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1963, 0.4).
narrative_ontology:measurement(mone_be_t1968, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1968, 0.58).
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1971, 0.72).
narrative_ontology:measurement(mone_be_t1973, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1973, 0.76).
narrative_ontology:measurement(mone_be_t1976, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1976, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t1958, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1958, 0.35).
narrative_ontology:measurement(mone_su_t1963, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1963, 0.42).
narrative_ontology:measurement(mone_su_t1968, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1968, 0.5).
narrative_ontology:measurement(mone_su_t1971, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1971, 0.6).
narrative_ontology:measurement(mone_su_t1973, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1973, 0.63).
narrative_ontology:measurement(mone_su_t1976, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1976, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__overdetermined_composite_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle__punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle__triffin_inevitability_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the monetary_anchor_principle kernel, decomposed per the ε-invariance principle: the natural-language label 'why did Bretton Woods collapse' conflates at least three structurally distinct causal claims. overdetermined_composite_reading (this file) claims multiple independently-sufficient structural streams converged; punctuated_swap_reading claims a discrete, contingent institutional decision on a specific date; triffin_inevitability_reading isolates the Triffin dilemma alone as the necessary and sufficient structural cause. Each carries its own ε, its own beneficiary/victim structure, and its own claimed type, linked here via affects_constraints rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
