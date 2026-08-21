% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Gold Standard Collapse: Overdetermined Composite Reading
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'overdetermined composite' reading
 *   of the monetary anchor principle, focusing on the collapse of the Bretton
 *   Woods gold standard. It posits that the collapse was an inevitable
 *   outcome of multiple, converging structural pressures: the Triffin dilemma
 *   (inherent contradiction of a reserve currency under gold convertibility),
 *   escalating Vietnam War deficits, the prevailing Keynesian policy
 *   consensus favoring active demand management, and increasing technological
 *   capital mobility. These factors collectively made the maintenance of gold
 *   convertibility unsustainable by the late 1960s, leading to its eventual
 *   abandonment in 1971.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, 0.85).
domain_priors:suppression_score(monetary_anchor_principle__overdetermined_composite_reading, 0.9).
domain_priors:theater_ratio(monetary_anchor_principle__overdetermined_composite_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(monetary_anchor_principle__overdetermined_composite_reading, "Gold Standard Collapse: Overdetermined Composite Reading").
narrative_ontology:topic_domain(monetary_anchor_principle__overdetermined_composite_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__overdetermined_composite_reading, '5178c9b3-db4f-45e4-b8d0-1c1e80d20010').
narrative_ontology:cs_kernel_codification('5178c9b3-db4f-45e4-b8d0-1c1e80d20010', formalized).
narrative_ontology:cs_authority_grounding('5178c9b3-db4f-45e4-b8d0-1c1e80d20010', lineage).
narrative_ontology:cs_interpretation_layer_present('5178c9b3-db4f-45e4-b8d0-1c1e80d20010').
narrative_ontology:cs_reading_relation('5178c9b3-db4f-45e4-b8d0-1c1e80d20010', monetary_anchor_principle__punctuated_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('5178c9b3-db4f-45e4-b8d0-1c1e80d20010', monetary_anchor_principle__triffin_inevitability_reading, influences).
narrative_ontology:cs_axiom('5178c9b3-db4f-45e4-b8d0-1c1e80d20010', foundational, monetary_regime_collapse_is_multi_causal).
narrative_ontology:cs_axiom_status(monetary_regime_collapse_is_multi_causal, holdable).
narrative_ontology:cs_axiom_grounding('5178c9b3-db4f-45e4-b8d0-1c1e80d20010', monetary_regime_collapse_is_multi_causal, empirically_contingent).
narrative_ontology:cs_axiom('5178c9b3-db4f-45e4-b8d0-1c1e80d20010', foundational, structural_pressures_limit_policy_agency).
narrative_ontology:cs_axiom_status(structural_pressures_limit_policy_agency, holdable).
narrative_ontology:cs_axiom_grounding('5178c9b3-db4f-45e4-b8d0-1c1e80d20010', structural_pressures_limit_policy_agency, empirically_contingent).
narrative_ontology:cs_reference_frame('5178c9b3-db4f-45e4-b8d0-1c1e80d20010', bretton_woods_gold_exchange_standard).
narrative_ontology:cs_drift_state('5178c9b3-db4f-45e4-b8d0-1c1e80d20010', late_1960s_collapse, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('5178c9b3-db4f-45e4-b8d0-1c1e80d20010', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, state_fiscal_capacity).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, monetary_discipline).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, international_creditors).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, domestic_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefited from the removal of the gold convertibility constraint, gaining flexibility for deficit financing and monetary expansion to fund domestic programs and international conflicts (e.g., Vietnam War).
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, state_fiscal_capacity, beneficiary,
    institutional, generational, arbitrage, global).

% Suffered from the loss of a hard monetary anchor, leading to increased inflationary pressures and reduced long-term stability in global financial markets. This 'agent' represents the abstract principle of sound money.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, monetary_discipline, payer,
    analytical, civilizational, trapped, universal).

% Held large dollar reserves, which were nominally convertible to gold. The collapse of convertibility meant their assets were devalued relative to gold, and they lost a key check on US monetary policy.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, international_creditors, payer,
    powerful, biographical, constrained, global).

% Experienced the effects of inflation as the monetary supply expanded without the gold constraint, eroding purchasing power and savings.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, domestic_citizens, payer,
    moderate, biographical, constrained, national).

% Provided the intellectual framework that justified active fiscal and monetary policy, contributing to the policy consensus that prioritized employment and growth over fixed exchange rates and gold convertibility.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, keynesian_economists, agenda_setter,
    organized, biographical, mobile, global).

% Argued for the preservation of the gold standard and monetary stability, but their voices were increasingly marginalized as structural pressures and policy priorities shifted towards flexibility.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, gold_standard_advocates, excluded,
    organized, generational, constrained, global).

% Managed the Bretton Woods system, attempting to balance domestic policy goals with international convertibility obligations, eventually succumbing to the pressures that led to the system's collapse.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, central_banks, agenda_setter,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The gold standard coordinated international trade and finance by providing a stable, convertible currency and fixed exchange rates, facilitating global economic interaction.
% TRANSFER_FUNCTION: The collapse transferred the constraint of gold convertibility from national fiscal and monetary policy, allowing for greater flexibility in managing domestic economies, but also shifting the burden of adjustment to exchange rates and inflation.
% ABSENT_VOICES: Advocates for strict monetary discipline and those who prioritized long-term currency stability over short-term fiscal flexibility were increasingly sidelined as the structural pressures mounted and policy consensus shifted.
% DISAPPEARANCE_RATIONALE: The collapse of the gold standard fundamentally reshaped the global monetary system, moving from fixed exchange rates and gold convertibility to a system of floating fiat currencies. This altered international trade, capital flows, and the tools available for national economic policy, leading to a significant reorganization of global finance.
% FOUNDING_PROBLEM: The gold standard was established to provide a stable, internationally recognized monetary anchor, prevent inflation, and facilitate international trade by ensuring currency convertibility and fixed exchange rates.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians widely corroborate the inherent contradictions and pressures faced by the gold standard in the post-WWII era, particularly the Triffin dilemma and the inability to reconcile domestic policy goals with fixed exchange rates. This corroboration comes from academic research, historical documents, and international financial institutions.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__overdetermined_composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__overdetermined_composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(monetary_anchor_principle__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__overdetermined_composite_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.85) reflects the significant cost imposed by the gold standard's rigidity on state fiscal capacity, which was increasingly constrained by the need for deficit spending. Suppression (0.9) is high because the structural pressures effectively suppressed any viable alternatives to abandoning gold convertibility, making the system's persistence dependent on increasingly unsustainable policy choices. Theater ratio is low (0.1) because the collapse was a genuine systemic failure, not a performative maintenance of a defunct function. Accessibility collapse (0.9) is high because the composite pressures foreclosed all practical alternatives to the eventual abandonment of gold convertibility. Resistance (0.5) was moderate, as some advocated for maintaining the gold standard, but the structural forces were overwhelming.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state fiscal capacity and Keynesian economists, the collapse was a necessary adaptation to evolving economic realities, freeing policy from an outdated constraint. From the perspective of monetary discipline and gold standard advocates, it represented a loss of stability and a dangerous embrace of inflationary policies. This reading emphasizes the structural inevitability, framing the 'choice' of 1971 as merely acknowledging an already determined outcome.
 *
 * DIRECTIONALITY LOGIC:
 *   State fiscal capacity is the primary beneficiary, as it gained immense flexibility once the gold constraint was removed, allowing for greater deficit spending. Monetary discipline, represented as an abstract principle, is the victim, as the removal of the gold anchor led to increased inflationary pressures. International creditors and domestic citizens were also victims, experiencing devaluation and inflation, respectively. Keynesian economists and central banks acted as agenda-setters, navigating and eventually facilitating the transition.
 *
 * MANDATROPHY ANALYSIS:
 *   The gold standard's original mandate to provide stable international monetary coordination became impossible to fulfill under the weight of conflicting demands (Triffin dilemma, fiscal expansion). The constraint did not so much atrophy as it was crushed by overdetermined structural pressures, leading to a forced resolution rather than a gradual decay. The classification as a Tangled Rope reflects the genuine coordination function that became entangled with the extractive pressure of maintaining an unsustainable system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_weight_of_factors,
    'What was the relative causal weight of each factor (Triffin dilemma, Vietnam War deficits, Keynesian consensus, capital mobility) in the gold standard''s collapse?',
    'Counterfactual historical analysis and econometric modeling attempting to isolate the impact of each factor, though definitive resolution is challenging due to their interconnectedness.',
    'A clearer understanding of causal weights would refine the narrative of inevitability and potentially highlight specific policy levers that, if adjusted, might have altered the timeline or nature of the collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_weight_of_factors, empirical, 'Determining the precise contribution of each overdetermining factor.').

omega_variable(
    inevitability_vs_agency,
    'To what extent was the collapse truly inevitable due to structural pressures, versus being the result of specific policy choices that could have been avoided or altered?',
    'Historical analysis of alternative policy proposals and their feasibility at the time, alongside theoretical modeling of the system''s stability under different policy regimes.',
    'If significant agency is found, the constraint might be re-read as more of a Snare (a choice for extraction) or a different type of Tangled Rope (where the ''coordination'' was more actively manipulated). If inevitability is confirmed, the current classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inevitability_vs_agency, conceptual, 'The balance between structural determinism and human agency in the gold standard''s demise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__overdetermined_composite_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t0, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mone_tr_t5, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement(mone_tr_t10, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(mone_tr_t15, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(mone_tr_t20, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(mone_tr_t25, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 25, 0.095).
narrative_ontology:measurement(mone_tr_t27, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 27, 0.1).

% Extraction over time
narrative_ontology:measurement(mone_be_t0, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(mone_be_t5, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(mone_be_t10, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(mone_be_t15, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 15, 0.8).
narrative_ontology:measurement(mone_be_t20, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 20, 0.83).
narrative_ontology:measurement(mone_be_t25, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 25, 0.84).
narrative_ontology:measurement(mone_be_t27, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 27, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t0, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(mone_su_t5, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(mone_su_t10, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(mone_su_t15, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 15, 0.85).
narrative_ontology:measurement(mone_su_t20, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 20, 0.88).
narrative_ontology:measurement(mone_su_t25, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 25, 0.89).
narrative_ontology:measurement(mone_su_t27, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 27, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__overdetermined_composite_reading, global_infrastructure).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, fiat_currency_regime).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, floating_exchange_rates).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'monetary_anchor_principle' kernel, focusing on the overdetermined composite of structural pressures leading to the gold standard's collapse. Sibling readings include 'punctuated_swap_reading' (emphasizing a discrete institutional choice) and 'triffin_inevitability_reading' (focusing solely on the Triffin dilemma).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
