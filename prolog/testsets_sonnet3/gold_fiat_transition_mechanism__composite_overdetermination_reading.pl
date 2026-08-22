% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__composite_overdetermination_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: gold_fiat_transition_mechanism__composite_overdetermination_reading
 *   human_readable: Gold-to-Fiat Transition as Overdetermined Structural Convergence
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   This story instantiates the composite_overdetermination reading of the
 *   gold_fiat_transition_mechanism kernel: the claim that the 1971-1973 shift
 *   from gold-anchored to fiat money was not a single mechanism swap (removal
 *   of an automatic constraint, or elimination of creditor veto power) but
 *   the convergence of independently evolving structural threads —
 *   telecommunications-enabled instant capital mobility, the mechanical
 *   collapse of Bretton Woods peg maintenance, labor bargaining power shifts
 *   already underway, and the gradual legal maturation of fiat legal tender
 *   enforcement. On this reading the Nixon Shock (August 1971 closing of the
 *   gold window) functions as a symbolic marker that historians and political
 *   actors retrospectively treat as the causal hinge, when in fact each
 *   thread would likely have produced substantial monetary system change even
 *   absent that specific announcement. The theater_ratio rises sharply around
 *   1971 and stays elevated because the Nixon Shock's continued treatment as
 *   THE causal event — in political rhetoric, popular economic history, and
 *   monetary policy legitimation narratives — is largely performative
 *   attribution laid over a multi-threaded structural process.
 *
 * KEY AGENTS:
 *   - central_bank_technocracy: institutional beneficiary of discretionary authority, did not engineer the convergence
 *   - multinational_treasury_operations: beneficiary via telecom-enabled capital mobility, one independent thread
 *   - fixed_income_savers: diffuse payer via inflation erosion, harmed by no single directed actor
 *   - gold_standard_era_creditor_nations: payer via lost redemption leverage, one of several parallel losses
 *   - economic_historians_overdetermination_school: analytical observer assembling the multi-causal account
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.42).
domain_priors:suppression_score(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.31).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__composite_overdetermination_reading, piton).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__composite_overdetermination_reading, "Gold-to-Fiat Transition as Overdetermined Structural Convergence").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__composite_overdetermination_reading, "monetary_economics/political_economy/history_of_economic_thought").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'f85ce02a-15ca-4b0f-8766-b128c2244da9').
narrative_ontology:cs_kernel_codification('f85ce02a-15ca-4b0f-8766-b128c2244da9', distributed).
narrative_ontology:cs_authority_grounding('f85ce02a-15ca-4b0f-8766-b128c2244da9', distributed).
narrative_ontology:cs_reading_relation('f85ce02a-15ca-4b0f-8766-b128c2244da9', gold_fiat_transition_mechanism__automatic_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('f85ce02a-15ca-4b0f-8766-b128c2244da9', gold_fiat_transition_mechanism__creditor_discipline_reading, coexists_with).
narrative_ontology:cs_axiom('f85ce02a-15ca-4b0f-8766-b128c2244da9', foundational, causal_singularity_of_monetary_transitions_is_a_narrative_artifact).
narrative_ontology:cs_axiom_status(causal_singularity_of_monetary_transitions_is_a_narrative_artifact, holdable).
narrative_ontology:cs_axiom_grounding('f85ce02a-15ca-4b0f-8766-b128c2244da9', causal_singularity_of_monetary_transitions_is_a_narrative_artifact, empirically_contingent).
narrative_ontology:cs_axiom('f85ce02a-15ca-4b0f-8766-b128c2244da9', secondary, distributional_effects_of_structural_convergence_are_irreducibly_polycentric).
narrative_ontology:cs_axiom_status(distributional_effects_of_structural_convergence_are_irreducibly_polycentric, holdable).
narrative_ontology:cs_axiom_grounding('f85ce02a-15ca-4b0f-8766-b128c2244da9', distributional_effects_of_structural_convergence_are_irreducibly_polycentric, empirically_contingent).
narrative_ontology:cs_reference_frame('f85ce02a-15ca-4b0f-8766-b128c2244da9', singular_mechanism_causal_historiography).
narrative_ontology:cs_drift_state('f85ce02a-15ca-4b0f-8766-b128c2244da9', post_1980s_monetary_historiography_consolidation, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f85ce02a-15ca-4b0f-8766-b128c2244da9', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, multinational_treasury_operations).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_financial_sector).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, central_bank_technocracy).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_income_savers).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_standard_era_creditor_nations).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, wage_indexed_labor_pre_1980s).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherited discretionary authority over money supply as an emergent consequence of the convergence, not as a designed seizure of power. Administers the resulting fiat regime, sets policy rates, and benefits from expanded tools, but did not engineer the underlying structural shifts (telecom capital mobility, Bretton Woods collapse, labor power realignment) that produced the opening.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, central_bank_technocracy, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__composite_overdetermination_reading, central_bank_technocracy, agenda_setter).

% Gained from instant cross-border capital flows enabled by telecommunications maturation, one of the independent structural threads. Can move capital across currency regimes at will, hedging or arbitraging currency risk that a gold-anchored system would have suppressed. Their gain is a byproduct of a technology shift, not of the fiat transition per se.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, multinational_treasury_operations, beneficiary,
    organized, biographical, arbitrage, global).

% Banks, dealers, and institutions trading in the dollar-denominated fiat system profit from expanded credit creation and floating exchange rate volatility. Their position resulted from the confluence of Bretton Woods collapse and legal tender maturation together, not from either alone.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_financial_sector, beneficiary,
    organized, generational, arbitrage, global).

% Held savings and fixed nominal claims that were eroded by the inflationary capacity fiat systems permit. They bore costs but were not targeted by any single actor — the erosion was a diffuse consequence of overlapping structural changes moving independently, none of which had this group's harm as its object.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_income_savers, payer,
    powerless, biographical, trapped, national).

% Nations like pre-1971 surplus-holding trading partners lost the redemption-threat leverage they held under Bretton Woods, but this loss arrived as one effect among several converging changes (labor bargaining shifts, telecom-enabled capital flight, legal tender maturation) rather than a discrete policy transfer aimed at them.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_standard_era_creditor_nations, payer,
    powerful, generational, constrained, continental).

% Labor bargaining power itself was one of the independent structural threads shifting during this period; workers whose wage agreements assumed monetary stability absorbed inflation-driven real wage erosion as the composite of forces played out, without any single actor directing the loss at them specifically.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, wage_indexed_labor_pre_1980s, payer,
    moderate, biographical, constrained, national).

% Scholars committed to single-cause narratives (automatic constraint removal, or creditor discipline collapse) are structurally excluded from this reading's frame because their explanatory model requires a unified causal node that this reading denies exists. They would object that overdetermination stories evade responsibility attribution.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, monetarist_historians, excluded,
    analytical, civilizational, analytical, global).

% Assemble the telecommunications, Bretton Woods, labor, and legal-tender threads as independently sufficient contributing causes, treating the Nixon Shock as a symbolic marker rather than the causal hinge. They analyze without occupying a beneficiary or payer seat in the arrangement itself.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, economic_historians_overdetermination_school, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__composite_overdetermination_reading, diffuse).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No single coordination function exists because no single mechanism exists: telecommunications infrastructure coordinated capital markets, Bretton Woods institutions coordinated exchange rate pegs (until they could not), labor contracts coordinated wage-price expectations, and legal tender statutes coordinated what counted as valid payment. Each thread solved its own distinct coordination problem independently.
% TRANSFER_FUNCTION: Multiple distinct transfers occurred in parallel rather than one: capital mobility transferred hedging advantage to mobile institutional actors; wage stagnation transferred real income from labor to capital as inflation outpaced indexation; loss of gold redemption transferred fiscal flexibility from creditor nations to the reserve-currency issuer. These transfers are not one flow and do not share one beneficiary.
% ABSENT_VOICES: Proponents of the automatic_constraint_reading and creditor_discipline_reading are excluded from this reading's frame by construction — this reading's core claim is that their unified causal node does not exist, so admitting their framework would contradict the reading's premise rather than merely disagreeing with its conclusion.
% DISAPPEARANCE_RATIONALE: If the composite-overdetermination reading were shown false, historians would not experience 'the world rearranging' in a material sense — money still flows as fiat currency regardless of which causal story is correct. What would change is attribution: policy discourse that currently distributes responsibility across technology, geopolitics, and labor markets would instead concentrate causal and moral responsibility on a single decision or a single class of actor, altering which parties are held accountable for post-1971 monetary outcomes.
% FOUNDING_PROBLEM: The founding problem this reading addresses is not a policy problem but a historiographical one: explaining why the 1971-1973 transition, when examined closely, does not reduce to any single decisive act, despite persistent narratives (both defenders and critics of fiat money) that treat the Nixon Shock as the singular causal event.
% FOUNDING_PROBLEM_CORROBORATION: Attested by economic historians outside any beneficiary group of the fiat transition itself (Barry Eichengreen's work on the multi-causal collapse of Bretton Woods, and international political economy scholarship on telecommunications-driven Eurodollar market growth predating 1971) — these are analytical observers with no stake in either the automatic-constraint or creditor-discipline framings, and their corroboration is independent of any party that gained from the transition's outcome.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__composite_overdetermination_reading, contested).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).
:- end_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high because no single actor engineered the outcome for concentrated gain — the story's whole point is that gains and losses distributed unevenly across multiple unrelated beneficiary and victim groups as a byproduct of independently moving parts, not as a designed transfer. Suppression is low-moderate (0.31): there was no active machinery suppressing alternatives to a unified fiat regime because there was no single decision point to defend, only structural drift across telecom, treaty, labor, and legal-tender domains that happened to converge. Theater_ratio is the most diagnostically important metric here and rises to 0.58 by the endpoint: this reflects the persistent public and political performance of treating the Nixon Shock as THE causal event, a performative simplification that grew MORE pronounced over time as it became institutionalized in textbooks, monetary policy legitimation rhetoric, and gold-standard revival advocacy — even though the underlying structural convergence account was already visible to contemporaries by the late 1960s (Euromarket growth, wage-price spiral dynamics, and peg-maintenance strain were all documented before 1971).
 *
 * DIRECTIONALITY LOGIC:
 *   No stakeholder here is a directed target or a directed beneficiary of a single mechanism because this reading denies there IS a single mechanism to be targeted by or benefit from. Central_bank_technocracy and multinational_treasury_operations sit near the beneficiary end not because the transition was engineered for them but because they were structurally positioned to capture the option value created by convergent flexibility (discretionary policy tools, capital mobility). Fixed_income_savers and wage_indexed_labor sit near the target end not because anyone extracted from them deliberately but because they held the least mobile, least hedged positions when multiple independent pressures (inflation capacity, wage-price dynamics, legal tender enforcement) converged on their fixed nominal claims simultaneously.
 *
 * MANDATROPHY ANALYSIS:
 *   The composite reading resists mandatrophy mislabeling by refusing the premise that would make either 'pure extraction' or 'pure coordination' analysis coherent: there is no single mandate to have outlived its function because there was no single founding act. The founding_problem is explicitly historiographical, not institutional — it is a claim about explanatory adequacy, not about a persisting organizational mandate. This is why claimed_type is piton rather than snare or tangled_rope: what persists post-1985 is not an extraction machine requiring active defense, but an inertial narrative structure (the Nixon-Shock-as-causal-node story) maintained through repetition in policy rhetoric and popular history long after its explanatory function has been superseded by better multi-causal accounts — genuine piton dynamics, theatrical maintenance of a simplified story rather than active extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_singularity_versus_composite_causation,
    'Is ''the gold-to-fiat transition'' a single event with a single causal mechanism (as the automatic_constraint_reading and creditor_discipline_reading both presuppose), or is it a retrospective label applied to a convergence of independently sufficient structural changes with no unified mechanism?',
    'Counterfactual historical analysis: examine whether each structural thread (telecom-enabled capital mobility, Bretton Woods peg strain, labor bargaining shifts, legal tender maturation) would have produced substantial fiat transition outcomes in the ABSENCE of the Nixon Shock specifically. Evidence from Eurodollar market growth data pre-1971, peg-defense cost trajectories, and comparative cases (other countries'' de facto float dates relative to their own domestic pressures) would bear on this.',
    'If the composite reading is correct, both sibling readings (automatic_constraint_reading, creditor_discipline_reading) misattribute causality to a non-existent unified transition, and their respective beneficiary/victim analyses are each partial slices of a larger multi-threaded redistribution rather than complete accounts. If a sibling reading is correct instead, this reading''s central claim collapses and its diffuse gain_flow/piton classification would need revision toward whichever reading''s beneficiary structure proves dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_singularity_versus_composite_causation, conceptual, 'Whether the kernel itself names a real singular event or a retrospective composite label — the central committer-frame question this reading exists to raise.').

omega_variable(
    nixon_shock_marker_versus_node_function,
    'Did the August 1971 Nixon Shock announcement itself have independent causal force (accelerating or shaping the transition''s specific timing and form), or was it purely a symbolic ratification of changes already structurally underway?',
    'Event-study analysis of capital flows, gold price behavior, and policy responses in the weeks/months immediately surrounding the announcement versus the preceding years of gradual peg strain; comparison with countries that floated before or after the US announcement for reasons unrelated to it.',
    'If the announcement had meaningful independent causal force (e.g., triggering specific capital flight patterns or accelerating other countries'' float decisions), the composite reading would need to concede a partial causal-node role to the Nixon Shock, softening its claim that it was ''symbolic marker, not causal node.'' If purely symbolic, the reading''s core distinguishing claim is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nixon_shock_marker_versus_node_function, empirical, 'Whether the Nixon Shock had any independent causal contribution beyond symbolizing pre-existing structural drift.').

omega_variable(
    diffuse_beneficiary_structure_stability,
    'Is the gain_flow genuinely diffuse across multiple unrelated beneficiary classes, or does closer distributional analysis reveal a dominant capturer (e.g., the reserve-currency financial sector) that the composite framing obscures by spreading attention across too many threads?',
    'Longitudinal wealth and capital-share analysis disaggregating gains to financial sector actors, multinational treasuries, and central banking institutions relative to gains/losses experienced by labor and fixed-income holders, to test whether the distributional picture is truly polycentric or whether one class captured disproportionate value.',
    'If a dominant capturer is identified, gain_flow should be revised from ''diffuse'' to a named stakeholder, and the claimed_type should be reconsidered toward tangled_rope or snare rather than piton — the composite-causation story would remain true at the mechanism level while the distributional story would need to concede concentration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diffuse_beneficiary_structure_stability, empirical, 'Whether the diffuse gain_flow claim holds under closer distributional scrutiny or conceals a concentrated beneficiary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__composite_overdetermination_reading, 1958, 1985).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1958, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1958, 0.12).
narrative_ontology:measurement(gold_tr_t1963, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1963, 0.19).
narrative_ontology:measurement(gold_tr_t1968, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1968, 0.28).
narrative_ontology:measurement(gold_tr_t1971, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1971, 0.55).
narrative_ontology:measurement(gold_tr_t1976, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1976, 0.51).
narrative_ontology:measurement(gold_tr_t1981, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1981, 0.56).
narrative_ontology:measurement(gold_tr_t1985, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1985, 0.58).

% Extraction over time
narrative_ontology:measurement(gold_be_t1958, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1958, 0.18).
narrative_ontology:measurement(gold_be_t1963, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1963, 0.24).
narrative_ontology:measurement(gold_be_t1968, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1968, 0.31).
narrative_ontology:measurement(gold_be_t1971, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1971, 0.36).
narrative_ontology:measurement(gold_be_t1976, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1976, 0.39).
narrative_ontology:measurement(gold_be_t1981, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1981, 0.41).
narrative_ontology:measurement(gold_be_t1985, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1985, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(gold_fiat_transition_mechanism__composite_overdetermination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, creditor_discipline_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the gold_fiat_transition_mechanism kernel. automatic_constraint_reading treats the transition as replacement of a material constraint with institutional discretion (single mechanism, single displaced constraint-type). creditor_discipline_reading treats it as a geopolitical power transfer from creditor nations to the reserve-currency issuer (single mechanism, single displaced party). This reading (composite_overdetermination_reading) denies both sibling readings' shared premise that a single causal mechanism exists at all, treating the transition instead as convergence of four independently sufficient structural threads with no unified beneficiary/victim structure. All three readings share the same underlying historical episode (1971-1973 gold window closure and float) but author different ε, different ClaimedType, and different beneficiary/victim sets because they disagree about what the constraint even IS at the mechanism level, not merely about whether it is good or bad.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
