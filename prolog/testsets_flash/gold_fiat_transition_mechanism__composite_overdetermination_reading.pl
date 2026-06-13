% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gold_fiat_transition_mechanism__composite_overdetermination_reading
 *   human_readable: Gold-Fiat Transition: Composite Overdetermination Reading
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   This constraint story describes the gold-fiat transition not as a
 *   singular event or policy choice, but as the overdetermined outcome of
 *   multiple converging structural changes: technological advancements in
 *   telecommunications, the inherent instability of the Bretton Woods system,
 *   shifts in labor market power, and the maturation of legal tender
 *   enforcement. The Nixon Shock is viewed as a symbolic marker rather than
 *   the sole causal node. This reading emphasizes the complexity and
 *   distributed agency in the transition, challenging simpler, monocausal
 *   narratives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.45).
domain_priors:suppression_score(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.6).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__composite_overdetermination_reading, rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__composite_overdetermination_reading, "Gold-Fiat Transition: Composite Overdetermination Reading").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__composite_overdetermination_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__composite_overdetermination_reading, '2138da0e-d432-4ccf-b773-e48d019246f7').
narrative_ontology:cs_kernel_codification('2138da0e-d432-4ccf-b773-e48d019246f7', implicit).
narrative_ontology:cs_authority_grounding('2138da0e-d432-4ccf-b773-e48d019246f7', distributed).
narrative_ontology:cs_reading_relation('2138da0e-d432-4ccf-b773-e48d019246f7', gold_fiat_transition_mechanism__automatic_constraint_reading, forecloses).
narrative_ontology:cs_reading_relation('2138da0e-d432-4ccf-b773-e48d019246f7', gold_fiat_transition_mechanism__creditor_discipline_reading, forecloses).
narrative_ontology:cs_axiom('2138da0e-d432-4ccf-b773-e48d019246f7', foundational, transition_as_overdetermined_convergence).
narrative_ontology:cs_axiom_status(transition_as_overdetermined_convergence, holdable).
narrative_ontology:cs_axiom_grounding('2138da0e-d432-4ccf-b773-e48d019246f7', transition_as_overdetermined_convergence, empirically_contingent).
narrative_ontology:cs_axiom('2138da0e-d432-4ccf-b773-e48d019246f7', secondary, nixon_shock_as_symbolic_marker).
narrative_ontology:cs_axiom_status(nixon_shock_as_symbolic_marker, holdable).
narrative_ontology:cs_axiom_grounding('2138da0e-d432-4ccf-b773-e48d019246f7', nixon_shock_as_symbolic_marker, empirically_contingent).
narrative_ontology:cs_reference_frame('2138da0e-d432-4ccf-b773-e48d019246f7', multi_causal_historical_process).
narrative_ontology:cs_drift_state('2138da0e-d432-4ccf-b773-e48d019246f7', contemporary_economic_historiography, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2138da0e-d432-4ccf-b773-e48d019246f7', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, central_banks).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, sovereign_governments).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, financial_sector).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_income_savers).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, labor_unions_pre_shift).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained significant discretion over monetary policy, no longer constrained by gold reserves. Administer the new fiat system, managing inflation and employment targets. Their power is now grounded in legal tender laws and public trust, not physical reserves.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, central_banks, agenda_setter,
    institutional, generational, constrained, national).

% Freed from balance-of-payments constraints imposed by gold convertibility, gaining fiscal flexibility. Can now finance deficits more easily through seigniorage and debt monetization, shifting the burden of adjustment.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, sovereign_governments, beneficiary,
    institutional, generational, constrained, national).

% Benefited from increased capital mobility enabled by telecommunications technology and the removal of fixed exchange rates. New financial instruments and markets emerged, increasing profitability and influence.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, financial_sector, beneficiary,
    organized, biographical, mobile, global).

% Experienced erosion of purchasing power due to inflation, which became a more persistent feature of fiat systems. Their savings, once implicitly backed by gold, were now subject to discretionary monetary policy and its inflationary consequences.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_income_savers, payer,
    powerless, biographical, trapped, national).

% Lost bargaining power as the ability of governments to inflate away debt and manage unemployment through monetary policy reduced the leverage of wage demands. The shift contributed to a long-term decline in real wages for some sectors.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, labor_unions_pre_shift, payer,
    organized, generational, constrained, national).

% Analyze the complex interplay of technological, economic, and political factors that led to the transition, challenging monocausal explanations and emphasizing the overdetermined nature of the shift.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, economic_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a new framework for international monetary relations and domestic economic management, coordinating national fiscal and monetary policies in the absence of a gold standard. It allowed for greater flexibility in responding to economic shocks.
% TRANSFER_FUNCTION: Transferred the constraint on money creation from physical gold reserves to institutional discretion. It also transferred economic adjustment burdens from external balance-of-payments discipline to domestic inflation and fiscal policy, with distributional effects on different economic classes.
% ABSENT_VOICES: Advocates for a return to a gold standard or other commodity-backed money, who argue for a non-discretionary, 'hard money' system, are largely excluded from mainstream policy debates, their arguments dismissed as anachronistic.
% DISAPPEARANCE_RATIONALE: The transition itself is a historical event; it cannot 'disappear'. Its effects are embedded in the current global financial system. If the *mechanisms* of the transition were to vanish, the world would not revert to a gold standard but would face a new, unprecedented financial crisis.
% FOUNDING_PROBLEM: The Bretton Woods system, based on fixed exchange rates and gold convertibility, faced increasing strain from growing international trade, capital flows, and national fiscal pressures, leading to a series of currency crises and an inability to adjust to global economic realities.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and central bank archives corroborate that the Bretton Woods system was indeed under severe strain and unsustainable by the early 1970s. The problem it was built to solve (post-WWII monetary stability) had evolved beyond its capacity. The current system, while having its own problems, is a direct descendant of the solutions to that dead problem.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__composite_overdetermination_reading, world_unchanged).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__composite_overdetermination_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'none', 1).

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
 *   The extractiveness (0.45) is moderate because the transition had mixed distributional effects, benefiting some (governments, financial sector) while imposing costs on others (savers, some labor groups). Suppression (0.6) reflects the necessary enforcement of legal tender laws and the suppression of alternative monetary systems. Theater ratio (0.1) is low, as the transition was a genuine structural shift, not primarily performative. Accessibility collapse (0.7) is high because the shift fundamentally altered the landscape of monetary policy and international finance, making a return to the prior system highly improbable. Resistance (0.3) was present from those who lost out, but not sufficient to prevent the systemic shift.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of central banks and governments, the transition was a necessary adaptation to economic realities, enabling greater flexibility and stability. From the perspective of savers and some labor groups, it represented a loss of economic security and a transfer of wealth. This reading, however, emphasizes the distributed nature of these effects, rather than a single, intentional act of extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Central banks and sovereign governments are beneficiaries due to increased policy discretion. The financial sector also benefited from new opportunities in a more flexible global capital market. Fixed-income savers and labor unions (pre-shift) are victims due to inflation and reduced bargaining power, respectively. The 'composite overdetermination' perspective implies that while there were beneficiaries and victims, no single agent or group 'caused' the transition for their sole benefit; rather, they adapted to and leveraged converging structural forces.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the transition as a simple 'snare' imposed by a single powerful actor. Instead, it frames it as a complex 'rope' (or perhaps a 'tangled rope' from some seats) that emerged from multiple, often independent, structural pressures. The 'mandate' was not a singular policy goal but a distributed adaptation to systemic instability. The question of mandatrophy is less about a single mandate outliving its function and more about whether the new system's distributed benefits outweigh its distributed costs, a question that remains contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_attribution_ambiguity,
    'To what extent was the gold-fiat transition a result of deliberate policy choices versus an inevitable outcome of converging structural forces?',
    'Counterfactual historical analysis: detailed modeling of alternative policy paths given the same structural conditions. Examination of primary source documents from policymakers to assess their perceived agency.',
    'If deliberate policy choices were more decisive, the extractiveness and suppression metrics might be higher, and the constraint might lean more towards a ''snare'' or ''tangled rope'' imposed by specific actors. If structural forces were truly overdetermining, the ''rope'' classification holds, emphasizing adaptation rather than imposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_attribution_ambiguity, empirical, 'Ambiguity regarding the balance of agency vs. structural inevitability in the transition.').

omega_variable(
    distributional_effects_measurement,
    'How precisely can the long-term distributional effects (beneficiaries vs. victims) of the composite transition be quantified across different economic classes and nations?',
    'Comprehensive econometric studies tracking wealth, income, and power shifts across decades, disaggregated by demographic and national groups. Requires overcoming data limitations and disentangling confounding factors.',
    'More precise quantification could shift the extractiveness metric up or down, potentially reclassifying the constraint from ''rope'' to ''tangled rope'' if asymmetric extraction is found to be more concentrated and severe than currently estimated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distributional_effects_measurement, empirical, 'Uncertainty in quantifying the full distributional impact of the multi-faceted transition.').

omega_variable(
    kernel_singularity_challenge,
    'Is ''the gold-fiat transition'' a coherent single kernel, or is it a label for a family of distinct, though related, structural changes?',
    'Conceptual analysis of historical narratives and economic models: if distinct causal pathways and outcomes can be cleanly separated, then the ''kernel'' itself might be a composite, requiring further decomposition into multiple constraints.',
    'If the kernel is truly composite, this reading''s claim of ''overdetermination'' is strengthened, and the other readings (automatic, creditor discipline) are revealed as attempts to impose a false singularity on a distributed phenomenon. This would reinforce the ''rope'' classification for this reading, as it accurately reflects the distributed nature of the change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_singularity_challenge, conceptual, 'Whether the ''gold-fiat transition'' is a singular event or a label for multiple converging changes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__composite_overdetermination_reading, 1960, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(gold_tr_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1965, 0.07).
narrative_ontology:measurement(gold_tr_t1970, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1970, 0.09).
narrative_ontology:measurement(gold_tr_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(gold_tr_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1980, 0.1).

% Extraction over time
narrative_ontology:measurement(gold_be_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1960, 0.3).
narrative_ontology:measurement(gold_be_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1965, 0.35).
narrative_ontology:measurement(gold_be_t1970, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(gold_be_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1975, 0.43).
narrative_ontology:measurement(gold_be_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1980, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1960, 0.4).
narrative_ontology:measurement(gold_su_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1965, 0.45).
narrative_ontology:measurement(gold_su_t1970, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(gold_su_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1975, 0.55).
narrative_ontology:measurement(gold_su_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1980, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__composite_overdetermination_reading, global_infrastructure).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, floating_exchange_rate_regime).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, central_bank_independence_doctrine).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, eurodollar_market_expansion).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'gold_fiat_transition_mechanism' kernel, emphasizing composite overdetermination. It challenges monocausal explanations found in sibling readings like 'automatic_constraint_reading' and 'creditor_discipline_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
