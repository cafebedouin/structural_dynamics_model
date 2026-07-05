% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__automatic_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__automatic_constraint_reading, []).

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
 *   constraint_id: gold_fiat_transition_mechanism__automatic_constraint_reading
 *   human_readable: Discretionary Monetary Authority Replacing the Gold-Convertibility Constraint
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   Prior to 1971, the U.S. dollar's convertibility to gold at a fixed rate
 *   meant the money supply was mechanically bounded: expanding the money
 *   supply beyond what gold reserves could back threatened a convertibility
 *   crisis, an automatic, non-discretionary check with zero degrees of
 *   freedom for policymakers. The Nixon Shock suspended convertibility, and
 *   over subsequent decades the automatic constraint was not replaced by
 *   nothing — it was replaced by discretionary central bank authority: an
 *   institutional apparatus (mandate, governance structure, enforcement via
 *   legal tender laws) that CAN be exercised with restraint but is not
 *   mechanically bound to be. This story reads that substitution as a change
 *   in constraint TYPE — material to institutional — with real distributional
 *   consequences: the class that benefited from the automatic floor
 *   (creditors, fixed-income holders) lost a structural protection, while the
 *   class that gained flexibility (monetary authorities, debtor treasuries)
 *   gained a discretionary tool usable for both genuine stabilization and
 *   quiet debt erosion.
 *
 * KEY AGENTS:
 *   - monetary_authorities: agenda_setter/beneficiary (institutional/arbitrage) — exercises the discretion the automatic constraint used to foreclose
 *   - creditor_class: payer (organized/constrained) — lost the automatic protection against debasement
 *   - fixed_income_savers: payer (powerless/trapped) — bears erosion with no exit and no voice
 *   - sovereign_fiscal_treasuries: beneficiary (institutional/constrained) — gained deficit-financing flexibility
 *   - future_generations: excluded (powerless/trapped) — inherits accumulated consequences with no representation in the founding decision
 *   - economic_historians: observer (analytical) — sees the structural substitution independent of any single reading's normative framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.68).
domain_priors:suppression_score(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.58).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__automatic_constraint_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__automatic_constraint_reading, "Discretionary Monetary Authority Replacing the Gold-Convertibility Constraint").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__automatic_constraint_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__automatic_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__automatic_constraint_reading, 'db04c2f6-4da4-47f7-b715-ff51c32cbce5').
narrative_ontology:cs_kernel_codification('db04c2f6-4da4-47f7-b715-ff51c32cbce5', distributed).
narrative_ontology:cs_authority_grounding('db04c2f6-4da4-47f7-b715-ff51c32cbce5', extraction).
narrative_ontology:cs_interpretation_layer_present('db04c2f6-4da4-47f7-b715-ff51c32cbce5').
narrative_ontology:cs_reading_relation('db04c2f6-4da4-47f7-b715-ff51c32cbce5', gold_fiat_transition_mechanism__creditor_discipline_reading, coexists_with).
narrative_ontology:cs_reading_relation('db04c2f6-4da4-47f7-b715-ff51c32cbce5', gold_fiat_transition_mechanism__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('db04c2f6-4da4-47f7-b715-ff51c32cbce5', foundational, physical_constraints_are_categorically_distinct_from_institutional_ones).
narrative_ontology:cs_axiom_status(physical_constraints_are_categorically_distinct_from_institutional_ones, holdable).
narrative_ontology:cs_axiom_grounding('db04c2f6-4da4-47f7-b715-ff51c32cbce5', physical_constraints_are_categorically_distinct_from_institutional_ones, conventional).
narrative_ontology:cs_axiom('db04c2f6-4da4-47f7-b715-ff51c32cbce5', secondary, discretionary_authority_necessarily_expands_absent_external_anchor).
narrative_ontology:cs_axiom_status(discretionary_authority_necessarily_expands_absent_external_anchor, holdable).
narrative_ontology:cs_axiom_grounding('db04c2f6-4da4-47f7-b715-ff51c32cbce5', discretionary_authority_necessarily_expands_absent_external_anchor, empirically_contingent).
narrative_ontology:cs_reference_frame('db04c2f6-4da4-47f7-b715-ff51c32cbce5', bretton_woods_convertibility_regime).
narrative_ontology:cs_drift_state('db04c2f6-4da4-47f7-b715-ff51c32cbce5', post_2008_unconventional_monetary_policy_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('db04c2f6-4da4-47f7-b715-ff51c32cbce5', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, sovereign_fiscal_treasuries).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, fixed_income_savers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, debtor_governments).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, debtor_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the money-creation policy that was formerly mechanically bounded by gold reserve ratios. Now exercises discretionary judgment over the money supply, interest rates, and, indirectly, the real value of outstanding debt and savings. Justifies the discretion as necessary for counter-cyclical stabilization and financial-crisis response; also captures the option value of being able to inflate away nominal liabilities and finance deficits without a hard convertibility check.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities, beneficiary).

% Gained the ability to run persistent deficits and finance them through debt monetization without triggering an automatic reserve drain or convertibility crisis. This flexibility funds counter-cyclical spending and long-horizon obligations (pensions, war finance, infrastructure) but also removes a hard external check on fiscal discipline.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, sovereign_fiscal_treasuries, beneficiary,
    institutional, generational, constrained, national).

% Formerly held a structural guarantee: currency convertibility to a fixed quantity of gold meant a definite floor under the real value of a claim on money. Under discretionary fiat authority, the value of that claim depends on decisions this class does not make and cannot bind. Can partially hedge (inflation-linked instruments, foreign-currency lending, gold holdings) but bears erosion when monetary authorities choose expansion. Exit is constrained rather than trapped — international capital markets offer partial arbitrage, but no return to an automatic constraint is available domestically.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class, payer,
    organized, biographical, constrained, global).

% Hold savings, pensions, and wages denominated in the fiat currency with limited capacity to hedge into alternative assets. Under the automatic constraint, the value of these holdings was protected from unilateral debasement; under discretionary authority, their purchasing power is subject to policy choices made by an institution they cannot exit or meaningfully influence.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, fixed_income_savers, payer,
    powerless, biographical, trapped, national).

% Benefit from the ability to inflate down the real burden of sovereign debt and avoid balance-of-payments crises previously triggered by reserve depletion. Also bear the cost of reduced external credibility and, at the limit, currency crises driven by loss of confidence rather than mechanical convertibility failure — a different, discretionary-era risk replacing the old one.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, debtor_governments, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__automatic_constraint_reading, debtor_governments, payer).

% Inherit whatever accumulated monetary and fiscal consequences (debt overhang, structural inflation expectations, institutional credibility) result from decades of discretionary policy choices. Have no voice in the transition or its ongoing administration; their interests are represented, if at all, indirectly through central bank mandates.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, future_generations, excluded,
    powerless, civilizational, trapped, national).

% Study the transition's structural mechanics, distinguishing the automatic-constraint-removal reading from rival accounts (creditor-discipline, composite-overdetermination). Have no material stake in the outcome but shape how the transition is remembered and taught.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__automatic_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A discretionary money-supply authority can respond to liquidity crises, financial panics, and demand shocks in ways a fixed gold-reserve ratio mechanically cannot — this is the genuine coordination function the transition purchased: countercyclical macroeconomic stabilization.
% TRANSFER_FUNCTION: Moves the power to determine the real value of money and debt from an automatic, non-negotiable physical constraint to a discretionary institutional process; this shifts real purchasing power from creditors and fixed-income holders (who lose the automatic floor) to monetary authorities and debtor governments (who gain the flexibility to expand the money supply and erode nominal liabilities).
% ABSENT_VOICES: Fixed-income savers and future generations bear the diffuse, slow-moving costs of discretionary policy but were not parties to the 1971 decision and have no direct channel to object to ongoing policy discretion; the creditor class had voice through bond markets and international pressure but no veto once convertibility was suspended.
% DISAPPEARANCE_RATIONALE: If discretionary central bank authority were replaced overnight by a restored automatic convertibility constraint, fiscal policy would be forced back into hard external discipline, countercyclical monetary response to crises would be sharply curtailed, and the real value of debt and savings would again be mechanically protected — a fundamentally different fiscal-monetary regime would result, not a cosmetic change.
% FOUNDING_PROBLEM: The Bretton Woods gold-exchange system tied the dollar to gold and other currencies to the dollar; by 1971, dollar liabilities held abroad vastly exceeded U.S. gold reserves, making the fixed convertibility promise physically impossible to honor at scale — the automatic constraint was breaking under its own arithmetic.
% FOUNDING_PROBLEM_CORROBORATION: IMF and Federal Reserve historical accounts, along with independent economic historians (e.g., Barry Eichengreen's analysis of Bretton Woods collapse), corroborate that the physical redemption constraint was already unsustainable by 1971 regardless of policy choice; this is attested by scholars and international institutions with no stake in preserving either the discretionary-authority arrangement or the gold-standard nostalgia narrative.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__automatic_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__automatic_constraint_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__automatic_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.68 by 2024) and rising because the discretionary authority has been used, over five decades, to erode the real value of nominal claims via sustained low-but-positive inflation and periodic large-scale monetary expansion — a pattern the automatic constraint mechanically prevented. Suppression (0.58) reflects that the institutional constraint is actively maintained: legal tender laws, central bank independence statutes, and the absence of any private convertibility alternative are enforced, not merely customary. Theater ratio (0.42) captures that a substantial share of central bank communication (inflation targeting frameworks, independence rhetoric) performs the appearance of a rule-bound, quasi-automatic system without the zero-discretion structure a true automatic constraint would have — the 2% inflation target functions partly as theatrical simulation of the old discipline. Accessibility collapse is high (0.71): once convertibility was suspended and legal tender enforcement matured, no private actor can restore an automatic personal hedge against currency discretion except through imperfect substitutes (gold holdings, foreign currency, inflation-linked bonds) that carry their own costs and risks — the original mechanical floor is simply gone. Resistance is moderate (0.55): the creditor class and hard-money advocates have mounted sustained political and intellectual resistance (gold standard revival movements, monetary rules advocacy) without success in reversing the institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary authorities and sovereign treasuries sit near the full-beneficiary end: they set the terms of the constraint's operation, capture the option value of discretion, and face no external convertibility check that could force unwanted expansion. The creditor class and fixed-income savers sit near the full-target end: they held a structural entitlement (the automatic floor) that was removed by an institutional decision they did not make and cannot reverse; their exit options are constrained-to-trapped depending on power (organized creditors can partially hedge internationally; individual savers largely cannot). Debtor governments are dual-positioned: beneficiaries of debt erosion but also exposed to a different discretionary-era risk (confidence-driven currency crisis) that the old system's crises did not present in the same form.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — Bretton Woods convertibility becoming arithmetically impossible — is genuinely dead; no one seriously proposes restoring 1960s-style dollar-gold convertibility at the old parity. But the arrangement did not simply resolve into a neutral replacement: it persists as an ongoing institutional structure (central bank discretion) whose original justification (crisis response to an unsustainable peg) has been extended into permanent, routine operation, including uses (sustained mild currency debasement, deficit monetization) that go well beyond the founding emergency. This is read as tangled_rope rather than snare: the coordination function (countercyclical stabilization capacity) is real and valuable, distinguishing it from pure extraction, but the same discretionary apparatus that enables stabilization also enables the asymmetric transfer from fixed-claim holders to discretion-holders — both functions run through the identical structure, which is the tangled_rope signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_automatic_vs_creditor_discipline,
    'Is the 1971 transition''s primary structural fact the removal of a physical money-creation constraint (this reading), or the removal of creditor veto power via redemption threat (the sibling creditor_discipline_reading)?',
    'These are not competing empirical claims resolvable by a single dataset — they are different structural framings of the same historical event, emphasizing different causal levers (physical/monetary vs. geopolitical/creditor-debtor power). Resolution would require agreement on which causal description is primary, which is itself a framing choice rather than a fact to be measured.',
    'Under this reading, the beneficiary/victim structure centers on monetary_authorities vs. creditor_class via the mechanism of discretion-over-debasement. Under the sibling reading, the same event is read as a geopolitical shift favoring reserve-currency-issuing debtor nations over creditor nations, with a different (though overlapping) victim/beneficiary map. Both readings can be simultaneously true as different structural lenses on one event; this story deliberately holds only one lens fixed per the ε-invariance principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_automatic_vs_creditor_discipline, conceptual, 'Whether the automatic-constraint-removal framing or the creditor-discipline framing is the primary structural description of the 1971 transition.').

omega_variable(
    single_causal_node_vs_composite_overdetermination,
    'Was the Nixon Shock a genuine causal node that changed the constraint''s type, or merely a symbolic marker for a convergence of independent structural changes (per the sibling composite_overdetermination_reading) that would have produced similar outcomes regardless of the specific 1971 announcement?',
    'Counterfactual historical analysis: would telecommunications-enabled capital mobility, Bretton Woods peg instability, and labor bargaining shifts have produced a functionally similar transition to discretionary monetary authority even absent the specific August 1971 decision? Comparative study of monetary regimes in economies that underwent similar convergent pressures without an equivalent single decisive event.',
    'If the composite reading is correct, this story''s framing of a discrete ''transition'' with clean beneficiary/victim structure overstates the causal weight of a single institutional decision relative to broader structural forces — the extraction pattern attributed here to discretionary authority might be better attributed to the underlying convergent conditions the authority merely formalized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(single_causal_node_vs_composite_overdetermination, conceptual, 'Whether the 1971 event is a genuine causal pivot or a symbolic marker for independently-caused structural convergence.').

omega_variable(
    genuine_stabilization_vs_extraction_ratio,
    'What proportion of discretionary monetary authority''s actual historical exercise has been genuine countercyclical stabilization versus revenue extraction via inflation (seigniorage) or debt erosion?',
    'Historical decomposition of central bank balance sheet actions and inflation outcomes into crisis-response episodes (2008 financial crisis, COVID-19) versus steady-state policy choices during non-crisis periods; comparison of realized inflation against stated targets over multi-decade windows.',
    'A high stabilization share would support weighting this constraint closer to rope; a high extraction share across non-crisis periods would support weighting it closer to snare. The tangled_rope classification authored here assumes both functions are substantially present and intertwined — resolving this ratio could shift the classification in either direction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_stabilization_vs_extraction_ratio, empirical, 'The empirical split between genuine stabilization use and extractive use of discretionary monetary authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__automatic_constraint_reading, 1971, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1971, 0.2).
narrative_ontology:measurement(gold_tr_t1981, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1981, 0.28).
narrative_ontology:measurement(gold_tr_t1991, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1991, 0.3).
narrative_ontology:measurement(gold_tr_t2001, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2001, 0.33).
narrative_ontology:measurement(gold_tr_t2011, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2011, 0.38).
narrative_ontology:measurement(gold_tr_t2024, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(gold_be_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1971, 0.35).
narrative_ontology:measurement(gold_be_t1981, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1981, 0.48).
narrative_ontology:measurement(gold_be_t1991, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1991, 0.5).
narrative_ontology:measurement(gold_be_t2001, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2001, 0.52).
narrative_ontology:measurement(gold_be_t2011, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2011, 0.62).
narrative_ontology:measurement(gold_be_t2024, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1971, 0.4).
narrative_ontology:measurement(gold_su_t1981, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1981, 0.45).
narrative_ontology:measurement(gold_su_t1991, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1991, 0.48).
narrative_ontology:measurement(gold_su_t2001, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2001, 0.5).
narrative_ontology:measurement(gold_su_t2011, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2011, 0.55).
narrative_ontology:measurement(gold_su_t2024, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__automatic_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_discipline_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the gold_fiat_transition_mechanism kernel. automatic_constraint_reading (this story) frames the transition as a physical-to-institutional constraint-type conversion with monetary_authorities as beneficiary and creditor_class as victim. creditor_discipline_reading frames the same event as removal of creditor veto power enabling debtor-nation fiscal flexibility, with an overlapping but distinct geopolitical beneficiary/victim structure. composite_overdetermination_reading denies the transition is a single causal node at all, treating Nixon Shock as symbolic rather than causal amid convergent structural forces (telecom-enabled capital mobility, Bretton Woods collapse, labor bargaining shifts, legal enforcement maturation). Each reading carries its own stable ε and its own stakeholder structure per the ε-invariance principle; none averages or reconciles with the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
