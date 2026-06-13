% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__automatic_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_automatic, []).

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
 *   constraint_id: gold_fiat_transition_mechanism__automatic_constraint_reading
 *   human_readable: Gold-to-Fiat Monetary Transition: Automatic Constraint Elimination
 *   domain: economic/political/institutional
 *
 * SUMMARY:
 *   The gold standard constrained monetary expansion automatically: central
 *   banks could not create money faster than gold reserves allowed, or they
 *   would face reserve depletion and be forced to contract. This reading
 *   claims the transition to fiat currency (formalized by the Nixon Shock in
 *   1971 and the Jamaica Accord in 1976) eliminated this automatic
 *   constraint, replacing it with discretionary central bank authority.
 *   Monetary authorities gained the power to expand the money supply without
 *   reserve discipline. Creditors and currency holders lost the automatic
 *   protection that redemption threat provided. The transition enabled
 *   counter-cyclical monetary policy but also enabled sustained inflation and
 *   seigniorage capture—making it a Tangled Rope: genuine coordination
 *   function (flexibility to manage crises) bundled with asymmetric
 *   extraction (authority gains discretion, creditors lose automatic
 *   discipline). The claim and metrics are authored independently: the
 *   constraint is CLAIMED as Tangled Rope (real coordination, real
 *   extraction) while metrics describe moderate suppression (discipline
 *   mechanisms persisted through capital markets) and moderate theater (some
 *   of the authority's noise around 'flexibility' masks rent collection). The
 *   engine measures this fit independently.
 *
 * KEY AGENTS:
 *   - monetary_authorities: The agenda-setter that gained discretion; benefits from the ability to expand money supply without reserve constraint.
 *   - creditor_class: The primary victim; lost automatic veto power through gold redemption threat and must rely on institutional discipline.
 *   - reserve_currency_issuers: Secondary beneficiary; the US and other major economies gained seigniorage through reserve-currency privilege without reserve depletion discipline.
 *   - debtor_nations: Secondary beneficiary; can run deficits and monetize debt without immediate reserve pressure.
 *   - labor_and_wage_earners: Distributed payer; absorb inflation risk through real wage erosion if monetary expansion outpaces wage indexation.
 *   - gold_producing_nations: Excluded; lost monetary demand for their primary commodity and were not seated in the transition decision.
 *   - academic_economists: Observer seat; measure the causal structure and outcomes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.71).
domain_priors:suppression_score(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.42).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__automatic_constraint_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__automatic_constraint_reading, "Gold-to-Fiat Monetary Transition: Automatic Constraint Elimination").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__automatic_constraint_reading, "economic/political/institutional").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__automatic_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__automatic_constraint_reading, '41dbac45-5df1-40a9-97cd-2a135c143dcf').
narrative_ontology:cs_kernel_codification('41dbac45-5df1-40a9-97cd-2a135c143dcf', formalized).
narrative_ontology:cs_authority_grounding('41dbac45-5df1-40a9-97cd-2a135c143dcf', extraction).
narrative_ontology:cs_interpretation_layer_present('41dbac45-5df1-40a9-97cd-2a135c143dcf').
narrative_ontology:cs_reading_relation('41dbac45-5df1-40a9-97cd-2a135c143dcf', gold_fiat_transition_mechanism__creditor_discipline_reading, coexists_with).
narrative_ontology:cs_reading_relation('41dbac45-5df1-40a9-97cd-2a135c143dcf', gold_fiat_transition_mechanism__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('41dbac45-5df1-40a9-97cd-2a135c143dcf', foundational, automatic_constraint_primacy).
narrative_ontology:cs_axiom_status(automatic_constraint_primacy, holdable).
narrative_ontology:cs_axiom_grounding('41dbac45-5df1-40a9-97cd-2a135c143dcf', automatic_constraint_primacy, empirically_contingent).
narrative_ontology:cs_axiom('41dbac45-5df1-40a9-97cd-2a135c143dcf', foundational, discretion_enables_debasement_risk).
narrative_ontology:cs_axiom_status(discretion_enables_debasement_risk, holdable).
narrative_ontology:cs_axiom_grounding('41dbac45-5df1-40a9-97cd-2a135c143dcf', discretion_enables_debasement_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('41dbac45-5df1-40a9-97cd-2a135c143dcf', gold_standard_automatic_discipline).
narrative_ontology:cs_drift_state('41dbac45-5df1-40a9-97cd-2a135c143dcf', post_nixon_shock_fiat_regime, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('41dbac45-5df1-40a9-97cd-2a135c143dcf', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, currency_reserve_holders).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__automatic_constraint_reading, 'none', 1).

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
 *   Extractiveness rises from 0.15 (early Bretton Woods, 1944) to 0.71 (post-1973 fiat regime) as the constraint's binding power shifts from automatic (gold reserves) to institutional (central bank discretion). The 1973 acceleration (0.68) marks the Nixon Shock and the formalization of the shift. Theater ratio rises to a peak in the 1970s (0.31 at 1973) when inflation acceleration forced authorities to articulate policy rationales, then stabilizes lower (0.28 at 1980) as inflation fighting became the stated focus—the noise around 'flexibility' recedes when discipline becomes the public narrative. Suppression requirement peaks at 0.44 (1973) when capital controls and wage-price interventions were deployed to manage the transition shock, then moderates slightly (0.42 at 1980) as markets adjusted and capital mobility rebalanced. The one shared measurement grid ensures every metric is authored at every time point, preventing misalignment. The metrics describe a constraint that operated as automatic (low suppression at outset) then required increasing institutional enforcement as authorities took discretion and creditors resisted through capital flight threat.
 *
 * PERSPECTIVAL GAP:
 *   The monetary authority (agenda_setter, institutional power) sees the transition as solving a real problem (rigid adjustment under gold) and enabling necessary flexibility for crisis management. From their seat, the constraint is Rope—genuine coordination benefit (crises can be managed counter-cyclically), low extractiveness relative to coordination value. The creditor (payer, powerful but powerful in a different domain—finance rather than monetary policy) sees the transition as an institutional shift that eliminated their automatic veto. From their seat, the constraint is Snare—pure extraction, with the 'flexibility' narrative serving as cover. The engine computes per-seat classifications from the same structural data; the divergence IS the measurement the corpus takes. One reading cannot be 'right'—the point is that the constraint produces different type experiences depending on which seat the agent occupies.
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary authorities sit at the beneficiary end (d near 0.0): they gained unrestricted discretion and collect seigniorage. Creditor class sits at the target end (d near 1.0): they lost automatic protection and must accept discretionary authority's decisions. Reserve-currency issuers are institutional beneficiaries (d = 0.1–0.2) with low exit cost (can always print their own currency). Labor and wage earners are organized payers (d = 0.7–0.8) with identity-locked exit (nominal wages are culturally and contractually sticky; they cannot easily exit into alternative compensation forms). Gold-producing nations are trapped victims (d = 0.9) with no recourse but to accept the loss of monetary demand. Academic economists are analytical observers (d = 0.5) with analytical exit—they can study the constraint without bearing its costs or collecting its benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rigid adjustment under gold prevented counter-cyclical policy) remains contested. Authorities claim it is live: crisis episodes (2008, 2020) demonstrate need for emergency expansion. Critics argue it is dead: the problem was solved by mid-century mechanisms (automatic stabilizers, Bretton Woods). The disappearance verdict is world_rearranges: if this institutional discretion system disappeared, capital flows and fiscal policy would reorganize around a new discipline mechanism (commodity backing, supranational standard, regional blocs). The mismatch (founding_problem_status=contested, disappearance_verdict=world_rearranges) suggests the constraint is not a natural law—it is institutional and contestable. Theater ratio rising through the 1970s then stabilizing suggests the transition from automatic to discretionary discipline was itself a source of performative activity (authorities had to justify flexibility against inflation concerns), but the performance quieted once inflation-fighting became the salient narrative. The constraint remains Tangled Rope, not degraded to Piton, because the coordination function (counter-cyclical flexibility) remains live even as the extraction (discretionary seigniorage) persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_primacy_automatic_vs_multiple,
    'Does this constraint represent the elimination of a SINGLE automatic physical discipline, or did multiple independent structural factors (telecommunications, labor shifts, legal tender maturation, geopolitical realignment) converge such that the gold standard would have collapsed regardless?',
    'Counterfactual historical analysis: what would have happened to the gold standard without post-WWII telecommunications, labor bargaining power growth, and US geopolitical dominance? Did the automatic constraint become unenforceable before it was formally abandoned?',
    'If the constraint was genuinely automatic and sufficient to discipline monetary expansion, then the reading''s claim of high extractiveness (authorities gained discretion over a binding constraint) holds. If multiple factors made the constraint inoperable first, then the transition was less a swap of constraints and more a formalization of already-eroded discipline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_primacy_automatic_vs_multiple, conceptual, 'Whether the automatic constraint was the primary causal driver of the transition or one factor among several converging structural changes').

omega_variable(
    creditor_discipline_residual,
    'Did the elimination of automatic gold discipline completely eliminate creditor veto power, or did creditors retain discipline through capital flight threat, credit rationing, and political influence?',
    'Empirical study of central bank decision-making: how much weight did feedback from creditor markets and actual capital flight episodes play in constraining monetary expansion? If capital flight and credit spreads were binding ex-post constraints, the discretion was always contingent.',
    'If creditor discipline remained substantial through market mechanisms, the constraint did not fully shift from automatic to institutional—institutional constraints remain present, just less automatic. The extraction value (epsilon) would be lower, and the constraint would be more Rope than Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_discipline_residual, empirical, 'Whether institutional creditor discipline replaced automatic gold discipline or whether both coexisted').

omega_variable(
    discretion_abuse_vs_necessary_flexibility,
    'Is the extractiveness measured here (0.71) a feature of the constraint itself, or a measure of how authorities have USED the discretion gained? Would a normative ideal-type ''responsible discretion'' constrained by explicit inflation targets and credible commitment devices lower extractiveness?',
    'Comparative institutional analysis: do central banks with explicit inflation mandates and operational independence show lower extractiveness than those without? Has the Taylor rule, inflation targeting, and forward guidance made discretion more disciplined?',
    'If extractiveness is usage-dependent rather than structural, the constraint''s type and severity depend on the regime within which discretion operates. The same institutional freedom could support Rope (if disciplined by credibility) or Snare (if abused). The baseline reading treats discretion as extractive by default; evidence of effective self-discipline would lower epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(discretion_abuse_vs_necessary_flexibility, empirical, 'Whether discretionary monetary authority is inherently extractive or whether contemporary frameworks have constrained it sufficiently').

omega_variable(
    reading_specificity_automatic_vs_composite,
    'This reading claims the transition eliminated ONE automatic constraint. The sibling ''composite_overdetermination_reading'' claims multiple independent structural factors converged. Are these readings genuinely incompatible accounts of the same transition, or do they address different aspects of a single multifactorial event?',
    'Definitional clarity: if ''automatic constraint'' is narrowly defined as ''physical enforcement mechanism independent of institutional will,'' then the gold standard is one such constraint. But if the reading is actually claiming ''the primary causal driver of the transition was the automatic constraint becoming unmanageable,'' then composite factors become relevant to the truth of the claim.',
    'If the readings are claims about causal primacy, they forecloses one another: only one mechanism can be primary. If they address different aspects (one reading explains the institutional mechanism change, the other explains what made the change necessary), they coexist. The resolution determines whether the reading_relations edge is forecloses or coexists_with.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_specificity_automatic_vs_composite, conceptual, 'Whether the automatic constraint reading and the composite overdetermination reading are logically incompatible or address complementary aspects of the same event').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__automatic_constraint_reading, 1944, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1944, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1944, 0.08).
narrative_ontology:measurement(gold_tr_t1950, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(gold_tr_t1960, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1960, 0.14).
narrative_ontology:measurement(gold_tr_t1970, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1970, 0.22).
narrative_ontology:measurement(gold_tr_t1973, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1973, 0.31).
narrative_ontology:measurement(gold_tr_t1980, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1980, 0.28).

% Extraction over time
narrative_ontology:measurement(gold_be_t1944, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1944, 0.15).
narrative_ontology:measurement(gold_be_t1950, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1950, 0.22).
narrative_ontology:measurement(gold_be_t1960, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1960, 0.38).
narrative_ontology:measurement(gold_be_t1970, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1970, 0.54).
narrative_ontology:measurement(gold_be_t1973, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1973, 0.68).
narrative_ontology:measurement(gold_be_t1980, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1980, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1944, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1944, 0.25).
narrative_ontology:measurement(gold_su_t1950, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1950, 0.28).
narrative_ontology:measurement(gold_su_t1960, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1960, 0.32).
narrative_ontology:measurement(gold_su_t1970, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1970, 0.39).
narrative_ontology:measurement(gold_su_t1973, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1973, 0.44).
narrative_ontology:measurement(gold_su_t1980, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1980, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__automatic_constraint_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.18).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, fiat_currency_credibility).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, reserve_currency_privilege).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, inflation_tax_distribution).

% DUAL FORMULATION NOTE:
% Part of the gold_fiat_transition_mechanism kernel family (three readings: automatic_constraint_reading, creditor_discipline_reading, composite_overdetermination_reading). Each reading has a different ε value and different beneficiary/victim structure because each identifies a different primary causal mechanism. The automatic_constraint_reading is mechanistic (focus on the swap of constraint types). The creditor_discipline_reading is geopolitical (focus on power redistribution). The composite_overdetermination_reading is historical-sociological (focus on multiple convergent factors). All three address the same event but from structurally distinct analytical frames.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
