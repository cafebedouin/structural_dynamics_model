% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__option_value_preserving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__option_value_preserving, []).

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
 *   constraint_id: acceptable_risk_energy__option_value_preserving
 *   human_readable: Option-Value-Preserving Acceptable Risk Framework
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of a contested kernel about
 *   acceptable risk in energy policy. The kernel conflict: what does
 *   'acceptable risk' mean when energy pathways carry different risk types
 *   (catastrophic tail probability vs. expected aggregate mortality vs.
 *   decision-lock risk) and the optimal choice depends on assumptions about
 *   deep uncertainties that remain unresolved? This
 *   reading—option-value-preserving—argues that acceptable risk requires
 *   maintaining multiple energy pathways (nuclear, fossil with CCS,
 *   renewables) simultaneously to preserve the flexibility to pivot if
 *   empirical outcomes diverge from current assumptions. Two sibling readings
 *   contest this: catastrophic-tail-dominant prioritizes avoiding
 *   low-probability catastrophic climate outcomes even at higher expected
 *   harm; expected-value-dominant minimizes mortality-per-TWh using uniform
 *   metrics. The three readings are structurally distinct constraints with
 *   different beneficiary/victim sets, different suppression mechanisms, and
 *   different claims about what 'acceptable' means. This constraint models
 *   ONLY the option-value-preserving reading, not the contested kernel as a
 *   whole.
 *
 * KEY AGENTS:
 *   - option_value_preserving_policymakers: agenda-setters maintaining portfolio diversification, moderate institutional power, generational time horizon, constrained exit (tied to energy policy consensus)
 *   - future_decision_makers: beneficiaries (analytical seat) gaining optionality to pivot if uncertainty resolves; analytical power, unbounded time horizon
 *   - modular_energy_portfolio_investors: beneficiaries with powerful institutional position and mobile exit (can reallocate across pathways without regulatory foreclosure)
 *   - advocates_for_rapid_decarbonization: victims bearing opportunity cost of delayed phase-out; organized power, constrained exit, biographical horizon
 *   - advocates_for_nuclear_elimination: victims bearing cost of continued nuclear viability; moderate power, constrained exit, biographical horizon
 *   - coal_dependent_regions: victims trapped in liminal status—coal neither phased out nor transitioned; moderate power, trapped exit (regional economic dependence)
 *   - catastrophic_risk_advocates (excluded): would prioritize tail-risk prevention over optionality; organized power, constrained exit, generational horizon
 *   - expected_value_minimizers (excluded): would apply mortality metrics uniformly; organized power, constrained exit, biographical horizon
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__option_value_preserving, 0.52).
domain_priors:suppression_score(acceptable_risk_energy__option_value_preserving, 0.48).
domain_priors:theater_ratio(acceptable_risk_energy__option_value_preserving, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, extractiveness, 0.52).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__option_value_preserving, rope).
narrative_ontology:human_readable(acceptable_risk_energy__option_value_preserving, "Option-Value-Preserving Acceptable Risk Framework").
narrative_ontology:topic_domain(acceptable_risk_energy__option_value_preserving, "risk_assessment/energy_policy/decision_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__option_value_preserving, 'ba1aaa7c-c183-4d97-a46f-97d258d6d8db').
narrative_ontology:cs_kernel_codification('ba1aaa7c-c183-4d97-a46f-97d258d6d8db', distributed).
narrative_ontology:cs_authority_grounding('ba1aaa7c-c183-4d97-a46f-97d258d6d8db', expertise).
narrative_ontology:cs_interpretation_layer_present('ba1aaa7c-c183-4d97-a46f-97d258d6d8db').
narrative_ontology:cs_reading_relation('ba1aaa7c-c183-4d97-a46f-97d258d6d8db', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('ba1aaa7c-c183-4d97-a46f-97d258d6d8db', acceptable_risk_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_axiom('ba1aaa7c-c183-4d97-a46f-97d258d6d8db', foundational, uncertainty_irreducibility_preserves_optionality_value).
narrative_ontology:cs_axiom_status(uncertainty_irreducibility_preserves_optionality_value, holdable).
narrative_ontology:cs_axiom_grounding('ba1aaa7c-c183-4d97-a46f-97d258d6d8db', uncertainty_irreducibility_preserves_optionality_value, instrumental).
narrative_ontology:cs_axiom('ba1aaa7c-c183-4d97-a46f-97d258d6d8db', foundational, decision_lock_cost_exceeds_path_selection_cost).
narrative_ontology:cs_axiom_status(decision_lock_cost_exceeds_path_selection_cost, holdable).
narrative_ontology:cs_axiom_grounding('ba1aaa7c-c183-4d97-a46f-97d258d6d8db', decision_lock_cost_exceeds_path_selection_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('ba1aaa7c-c183-4d97-a46f-97d258d6d8db', multi_pathway_portfolio_maintenance).
narrative_ontology:cs_drift_state('ba1aaa7c-c183-4d97-a46f-97d258d6d8db', contemporary_2024, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ba1aaa7c-c183-4d97-a46f-97d258d6d8db', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, future_decision_makers).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, modular_energy_portfolio_investors).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, advocates_for_rapid_decarbonization).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, advocates_for_nuclear_elimination).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, coal_dependent_regions).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__option_value_preserving, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_energy__option_value_preserving, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__option_value_preserving_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__option_value_preserving, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__option_value_preserving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.52 at interval end, rising from 0.38 at start. The rise reflects increasing opportunity cost becoming visible as renewables scale and fossil pathways demonstrate competitive weakness (if climate remains stable, if technology costs fall as projected, if grids adapt to high renewable penetration—all uncertain). The framework's core extraction is that advocates for any major pathway change (decarbonization acceleration, nuclear elimination, catastrophic-risk prioritization) bear the cost of deferred decision in exchange for future optionality they may never use. Theater ratio is stable at ~0.31: the framework genuinely functions (maintains regulatory and investment structures for multiple pathways), but a growing fraction of that maintenance is performative—maintaining coal and nuclear viability despite competitive weakness, articulated as 'preserving options' rather than responding to live policy uncertainty. Suppression is moderate (0.48) and stable: the framework suppresses both rapid-decarbonization and catastrophic-risk-prioritizing coalitions through institutional consensus and research funding allocation patterns, but neither is fully coercively blocked (they retain organizing capacity and political voice, just not policy dominance). Measurement grid is shared across all three metrics at every time point, enabling temporal coherence analysis. Basis values note where measurements rest on empirical observation (post-2010) vs. projection forward from competition scenarios.
 *
 * PERSPECTIVAL GAP:
 *   The option-value-preserving policymaker seat perceives this as genuine coordination: 'we are collectively protecting ourselves from premature lock-in by maintaining multiple viable pathways.' The rapid-decarbonization seat perceives extraction: 'they are using uncertainty as a pretext to continue fossil fuel viability and prevent the investment concentration that would accelerate zero-carbon transition.' The nuclear-elimination seat perceives pure extraction: 'the framework preserves nuclear as a viable policy option against our explicit preference and at cost of continued research funding and permitting complexity.' The coal-dependent-region seat perceives liminal entrapment: 'we are neither phased out (which would trigger transition resources) nor treated as viable (which would trigger investment). We are kept in permanent backup status.' From future decision-makers' analytical perspective, the constraint functions as genuine optionality insurance if uncertainty about climate and technology trajectories remains high; it functions as rent-seeking by energy incumbents if the uncertainty is smaller than claimed and the pathway optimization becomes obvious before the optionality is exercised. The engine computes these divergent perceptions from the structural data; the claim that the constraint is 'rope' (my authored position) may compute differently at different seats depending on their power atoms and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint benefits future decision-makers (analytical seat, high beneficiary directionality ~0.1) and modular energy investors (powerful institutional seat, moderate directionality ~0.35). It extracts from rapid-decarbonization advocates (organized seat, high target directionality ~0.8), nuclear-elimination advocates (moderate seat, high target directionality ~0.75), and coal-dependent regions (moderate powerless-adjacent seat, trapped exit, maximum target directionality ~0.95). The suppression mechanism is moderate because both extraction cohorts (rapid decarbonization and nuclear elimination) retain organizing capacity and political voice; they are constrained from dominance but not silenced. Policy-maker seats compute as symmetric (0.5): they benefit from portfolio stability they engineer, but bear cost of defending the framework against both excluded coalitions continuously. The framework's stability since 2010 despite rising renewable competitiveness and accelerating climate science suggests either that beneficiary alignment (investors, future flexibility) is stronger than victim organizing, or that the excluded coalitions lack political power to overthrow the consensus, or both.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—deep uncertainty about energy pathways and need to preserve flexibility—was live at the constraint's inception (~1995-2010, climate science uncertainty was substantially higher, technology roadmaps were more divergent). As of 2024, the empirical status is contested. The rapid-decarbonization coalition argues the founding problem is dead or dying: climate sensitivity estimates have converged, renewable cost curves have materialized as predicted, grid integration challenges are proving manageable, and the optimal path (rapid renewable deployment + nuclear as supplement, not coal as option) is now clear. The option-value coalition argues the founding problem is live: tipping points in Earth systems remain uncertain, renewable storage at planetary scale is still unproven, and geopolitical supply-chain risks for rare earths justify maintaining coal and nuclear backup. A classical mandatrophy signal would be: founding_problem_status = 'dead' but disappearance_verdict = 'world_rearranges' (the arrangement persists despite the problem that justified it being gone). Here the mandatrophy question is softer: if we empirically resolve that the optimal energy pathway is obvious and the optionality was never exercised because it was never needed, the constraint has become Piton-like (maintained by inertia despite its founding justification eroding). The measurement series showing extractiveness rising while theater ratio stays flat suggests the framework is shifting from 'genuinely preserving options' toward 'using optionality rhetoric to protect incumbent pathways'—a slow mandatrophy trajectory. No decisive mandatrophy verdict yet (founding_problem_status = 'contested' captures this), but the T17 temporal trigger (mountain_extraction_accumulation or tangled_rope_rent_accumulation) would flag investigation if extractiveness continues rising while founding problem clarity remains contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_uncertainty_magnitude,
    'How large is the irreducible uncertainty about energy pathways, climate sensitivity, and technology trajectories? Is it large enough to justify maintaining multiple pathways, or has empirical progress collapsed the decision space to one or two dominant options?',
    'Empirical resolution of climate sensitivity via ice-core data and paleo-climate reconstruction; technology cost curves materializing as predicted or diverging; large-scale grid integration experience with 80%+ renewable penetration; CCS and long-duration storage deployment outcomes.',
    'If uncertainty remains large (wide model divergence, climate tipping points unresolved, storage unproven), option preservation is genuine coordination. If uncertainty is small (convergent model consensus, climate sensitivity pinned down, storage cost-viable), option preservation becomes rent-seeking cover story and constraint reclassifies toward Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_uncertainty_magnitude, empirical, 'Whether the founding problem remains live or has been empirically resolved.').

omega_variable(
    kernel_reading_foreclosure,
    'Do the three kernel readings (option-value-preserving, catastrophic-tail-dominant, expected-value-dominant) logically foreclose each other within a single coherent decision framework, or do they coexist as valid positions held by different epistemic communities?',
    'Analytical reconstruction of the decision-theory premises each reading rests on: does option-value-preserving require assuming uncertainty irreducibility, or is it compatible with resolved uncertainty if new contingencies emerge? Does catastrophic-tail-dominance require treating low-probability outcomes as *overriding* expected value, or can it coexist with expected-value minimization if the tail outcome is also the expected-value-maximal outcome?',
    'If readings foreclose each other, one must be wrong and the others right—the kernel has a determinate correct reading. If they coexist, they are valid positions under different axioms and the constraint family remains genuinely contested (no single ''true'' reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the three kernel readings are logically incompatible or represent valid alternative framings of the decision problem.').

omega_variable(
    opportunity_cost_quantification,
    'What is the empirical opportunity cost of maintaining fossil and nuclear pathways viable while renewable deployment accelerates? How much slower does decarbonization proceed, or how much additional carbon is emitted, compared to a committed renewable-plus-limited-nuclear pathway?',
    'Comparative cost and emissions modeling: hold the option-value framework fixed, model energy system deployment under its constraints, compare emissions and transition speed to counterfactual committed-pathway scenarios.',
    'If opportunity cost is small (emissions pathway divergence within measurement error, deployment speed unchanged), the framework''s extraction is minimal. If opportunity cost is large (multi-gigatonne cumulative emissions difference, 5-10 year decarbonization delay), the extraction is substantial and victim claims are quantified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(opportunity_cost_quantification, empirical, 'The magnitude of the constraint''s actual extractive impact on decarbonization speed and cumulative emissions.').

omega_variable(
    identity_lock_suppression_mechanism,
    'Is the suppression of rapid-decarbonization advocates and coal-dependent regions structural (external barriers) or internalized (the groups have absorbed the optionality-preservation framing and now operate within it), or both?',
    'Post-policy-reversal observation: if the framework were dismantled and committed decarbonization or nuclear phase-out became law, would advocates continue to believe their former position or immediately embrace the new consensus? If they immediately embrace it, suppression was structural; if they maintain resistance, suppression was partly internalized.',
    'Structural suppression alone would persist as institutional constraint even if policy changed; internalized suppression would travel with the agent and maintain the framework''s logic even absent institutional enforcement. High internalization suggests identity-fusion to the optionality-preservation premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_suppression_mechanism, empirical, 'Whether measured suppression reflects external institutional barriers or absorbed normative commitments or both.').

omega_variable(
    modular_investor_beneficiary_asymmetry,
    'Do modular energy portfolio investors genuinely benefit from the option-value-preserving framework, or does their empirical investment behavior (capital flowing to renewable deployment, nuclear construction stopping, coal retirement accelerating despite framework permission) suggest the framework benefits institutional inertia rather than actual investor flexibility?',
    'Portfolio analysis of major energy investors: measure actual capital allocation by pathway, note divergence between framework permission (multiple pathways viable) and actual deployment (capital concentrating in renewables). If investors behave as though the framework did NOT exist (flowing capital to lowest-cost, lowest-carbon pathways), the beneficiary designation is misplaced.',
    'If investors are benefiting from portfolio maintenance they don''t actually exercise, the constraint may be purely extractive on victims (advocates, coal regions) with no genuine beneficiary collecting. Reclassifies toward Snare. If investors genuinely maintain optionality portfolios (capital hedged across all pathways), beneficiary designation stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(modular_investor_beneficiary_asymmetry, empirical, 'Whether the constraint''s nominal beneficiaries actually exploit the optionality it preserves or merely benefit from its existence regardless of use.').

omega_variable(
    reading_committer_kernel_identity,
    'This constraint is one reading of the acceptable_risk_energy kernel. Is the kernel itself well-defined, or is what counts as ''acceptable risk'' so contested that the kernel dissolves into three independent constraint claims rather than three readings of one kernel?',
    'Genealogical analysis: trace the three readings back to a common institutionalization moment (energy policy consensus ~1995-2010) and verify a shared commitment was established that all three readings purport to interpret. If no such shared moment exists, the readings are not variants of one kernel but independent constraint claims.',
    'If kernel is real (shared historical commitment), the readings coexist as alternative interpretations and the constraint family is properly unified. If kernel is constructed retroactively (three independent claims bundled as ''readings of one thing''), the framing is incoherent and should be split into three independent stories with no ''kernel'' language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_kernel_identity, conceptual, 'Whether the three sibling readings genuinely read from a shared kernel or represent independent constraint claims wrongly unified.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__option_value_preserving, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__option_value_preserving, theater_ratio, 0, 0.28).
narrative_ontology:measurement(acce_tr_t5, acceptable_risk_energy__option_value_preserving, theater_ratio, 5, 0.29).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_energy__option_value_preserving, theater_ratio, 10, 0.3).
narrative_ontology:measurement(acce_tr_t15, acceptable_risk_energy__option_value_preserving, theater_ratio, 15, 0.31).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_energy__option_value_preserving, theater_ratio, 20, 0.31).
narrative_ontology:measurement(acce_tr_t25, acceptable_risk_energy__option_value_preserving, theater_ratio, 25, 0.31).
narrative_ontology:measurement(acce_tr_t30, acceptable_risk_energy__option_value_preserving, theater_ratio, 30, 0.31).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_energy__option_value_preserving, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__option_value_preserving, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(acce_be_t5, acceptable_risk_energy__option_value_preserving, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_energy__option_value_preserving, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(acce_be_t15, acceptable_risk_energy__option_value_preserving, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_energy__option_value_preserving, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(acce_be_t25, acceptable_risk_energy__option_value_preserving, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(acce_be_t30, acceptable_risk_energy__option_value_preserving, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_energy__option_value_preserving, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__option_value_preserving, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(acce_su_t5, acceptable_risk_energy__option_value_preserving, suppression_requirement, 5, 0.44).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_energy__option_value_preserving, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(acce_su_t15, acceptable_risk_energy__option_value_preserving, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_energy__option_value_preserving, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(acce_su_t25, acceptable_risk_energy__option_value_preserving, suppression_requirement, 25, 0.48).
narrative_ontology:measurement(acce_su_t30, acceptable_risk_energy__option_value_preserving, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_energy__option_value_preserving, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__option_value_preserving, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__option_value_preserving, 0.18).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy__expected_value_dominant).

% DUAL FORMULATION NOTE:
% This constraint is one reading (option_value_preserving) of a contested kernel (acceptable_risk_energy). Two sibling readings instantiate the same kernel under different axioms: catastrophic_tail_dominant prioritizes low-probability catastrophic outcomes over expected harm; expected_value_dominant applies uniform mortality metrics to collapse all pathways into a single optimal choice. The three readings are structurally distinct constraints with different epsilon values, different beneficiary/victim structures, and different suppression mechanisms. The kernel unification is justified by the shared institutional moment (energy policy consensus ~1995-2010) that all three readings purport to interpret. This story models ONLY the option-value-preserving reading; see sibling stories for the other readings' epsilon values and beneficiary decompositions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
