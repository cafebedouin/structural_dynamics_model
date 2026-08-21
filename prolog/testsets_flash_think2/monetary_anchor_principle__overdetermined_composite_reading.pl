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
 *   human_readable: Fiat Monetary Regime (Overdetermined Emergence)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story describes the fiat monetary regime as the current
 *   'monetary anchor principle,' viewed through the lens of its
 *   overdetermined emergence from the collapse of the gold standard. The
 *   transition was not a single policy choice but an inevitable outcome of
 *   multiple structural pressures: the Triffin dilemma, Vietnam War deficits,
 *   a prevailing Keynesian policy consensus, and increasing technological
 *   capital mobility. This reading frames the resulting fiat system as a
 *   Tangled Rope, providing coordination for state fiscal capacity and global
 *   capital flows, but extracting from monetary discipline and savers through
 *   inflation.
 *
 * KEY AGENTS:
 *   - state_fiscal_capacity: Agenda-setter/Beneficiary (institutional/arbitrage)
 *   - central_banks: Agenda-setter/Beneficiary (institutional/mobile)
 *   - monetary_discipline_advocates: Payer/Excluded (organized/constrained)
 *   - savers: Payer (powerless/constrained)
 *   - fixed_income_earners: Payer (powerless/constrained)
 *   - international_investors: Beneficiary (powerful/arbitrage)
 *   - economic_historians: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, 0.85).
domain_priors:suppression_score(monetary_anchor_principle__overdetermined_composite_reading, 0.75).
domain_priors:theater_ratio(monetary_anchor_principle__overdetermined_composite_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(monetary_anchor_principle__overdetermined_composite_reading, "Fiat Monetary Regime (Overdetermined Emergence)").
narrative_ontology:topic_domain(monetary_anchor_principle__overdetermined_composite_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__overdetermined_composite_reading, 'dd058186-abfd-48a5-9042-a84fda92b63c').
narrative_ontology:cs_kernel_codification('dd058186-abfd-48a5-9042-a84fda92b63c', implicit).
narrative_ontology:cs_authority_grounding('dd058186-abfd-48a5-9042-a84fda92b63c', extraction).
narrative_ontology:cs_interpretation_layer_present('dd058186-abfd-48a5-9042-a84fda92b63c').
narrative_ontology:cs_reading_relation('dd058186-abfd-48a5-9042-a84fda92b63c', monetary_anchor_principle__punctuated_swap_reading, forecloses).
narrative_ontology:cs_reading_relation('dd058186-abfd-48a5-9042-a84fda92b63c', monetary_anchor_principle__triffin_inevitability_reading, influences).
narrative_ontology:cs_axiom('dd058186-abfd-48a5-9042-a84fda92b63c', foundational, monetary_regimes_are_structurally_determined).
narrative_ontology:cs_axiom_status(monetary_regimes_are_structurally_determined, holdable).
narrative_ontology:cs_axiom_grounding('dd058186-abfd-48a5-9042-a84fda92b63c', monetary_regimes_are_structurally_determined, empirically_contingent).
narrative_ontology:cs_axiom('dd058186-abfd-48a5-9042-a84fda92b63c', secondary, multi_causal_systemic_collapse).
narrative_ontology:cs_axiom_status(multi_causal_systemic_collapse, holdable).
narrative_ontology:cs_axiom_grounding('dd058186-abfd-48a5-9042-a84fda92b63c', multi_causal_systemic_collapse, empirically_contingent).
narrative_ontology:cs_reference_frame('dd058186-abfd-48a5-9042-a84fda92b63c', gold_standard_inherent_instability).
narrative_ontology:cs_drift_state('dd058186-abfd-48a5-9042-a84fda92b63c', post_bretton_woods_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('dd058186-abfd-48a5-9042-a84fda92b63c', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, state_fiscal_capacity).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, central_banks).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, monetary_discipline).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, savers).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, fixed_income_earners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, international_investors).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, monetary_discipline_advocates).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policy_consensus).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__overdetermined_composite_reading, triffin_dilemma_validity).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__overdetermined_composite_reading, capital_mobility_constraint).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the flexibility to fund deficits through monetary expansion, unconstrained by gold reserves. Sets fiscal policy that influences monetary outcomes.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, state_fiscal_capacity, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__overdetermined_composite_reading, state_fiscal_capacity, beneficiary).

% Administers and enforces the fiat monetary system, managing interest rates, money supply, and exchange rates. Benefits from expanded policy tools and seigniorage.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, central_banks, agenda_setter,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__overdetermined_composite_reading, central_banks, beneficiary).

% Bear the cost of reduced monetary discipline, advocating for sound money and fixed exchange rates. Their arguments are often marginalized in policy debates.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, monetary_discipline_advocates, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__overdetermined_composite_reading, monetary_discipline_advocates, excluded).

% Experience erosion of purchasing power due to inflation, a consequence of the flexible fiat system. Their options for preserving wealth are limited and often carry higher risk.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, savers, payer,
    powerless, biographical, constrained, national).

% Suffer from inflation as their nominal incomes do not keep pace with rising prices, effectively transferring wealth to those who can inflate away debt.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, fixed_income_earners, payer,
    powerless, immediate, constrained, national).

% Benefit from the liquidity and flexibility of the fiat system, allowing for rapid capital movement and arbitrage opportunities across different national monetary policies.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, international_investors, beneficiary,
    powerful, immediate, arbitrage, global).

% Analyze the structural causes and consequences of the gold standard's collapse and the emergence of the fiat regime, providing an analytical perspective on the inevitability of the transition.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, economic_historians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a flexible global monetary system that allows for national macroeconomic stabilization, international capital mobility, and the financing of state expenditures, replacing the rigid gold standard.
% TRANSFER_FUNCTION: Transfers inflation risk from states to savers and fixed-income earners, and transfers seigniorage (the profit from issuing currency) to central banks and governments.
% ABSENT_VOICES: Strong proponents of a gold-backed currency or other fixed-exchange-rate regimes are largely excluded from mainstream policy discourse, as their proposals are deemed incompatible with modern fiscal and monetary needs.
% DISAPPEARANCE_RATIONALE: If the fiat monetary system vanished overnight, global trade and finance would collapse, leading to economic chaos as there would be no universally accepted medium of exchange or store of value, forcing a rapid and painful reorganization around new, likely localized, monetary anchors.
% FOUNDING_PROBLEM: The gold standard imposed rigid constraints on national fiscal policy, exacerbated international financial instability (Triffin dilemma), and proved incompatible with the demands of modern welfare states and global capital mobility.
% FOUNDING_PROBLEM_CORROBORATION: Central banks and most governments attest the problems are live, citing the need for flexible policy tools. Monetary discipline advocates and some economic historians argue that while old problems were 'solved,' new ones (inflation, financial instability) were created, and the original problems could have been addressed differently. Independent economic analysis supports both the historical pressures and the subsequent policy choices.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__overdetermined_composite_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__overdetermined_composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because the fiat system allows states to extract seigniorage and implicitly tax savers through inflation, a structural feature of the regime. Suppression is high (0.75) due to the active enforcement by central banks and legal tender laws, which effectively collapse alternatives to the dominant fiat currencies. Theater ratio is low (0.10) as the system is highly functional and its operations are not primarily performative. Accessibility collapse is high (0.80) because exiting the fiat system for alternatives (e.g., gold, cryptocurrencies) is difficult and carries significant costs for most actors. Resistance is moderate (0.40), coming from niche groups rather than a broad-based challenge to the system's legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   State fiscal capacity and central banks perceive the fiat system as a necessary and flexible coordination mechanism, enabling economic stability and growth. In contrast, savers and monetary discipline advocates experience it as an extractive mechanism that erodes wealth and undermines long-term financial stability. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State fiscal capacity and central banks are clear beneficiaries, gaining flexibility and seigniorage (low d). Savers and fixed-income earners are targets, bearing the costs of inflation (high d). Monetary discipline advocates are also targets, as their preferred system is suppressed. International investors benefit from liquidity and arbitrage opportunities (low d).
 *
 * MANDATROPHY ANALYSIS:
 *   The fiat monetary regime's mandate has shifted from the gold standard's goal of fixed convertibility and price stability to one of flexible macroeconomic management and financial stability (often interpreted as avoiding deflation). This reading prevents mislabeling the current system as a pure Snare by acknowledging its coordination functions for states and capital, while still highlighting its extractive properties. The 'overdetermined composite' explanation grounds the shift in structural inevitability rather than pure policy choice, which influences the perception of its legitimacy and potential for reform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inevitability_vs_choice_ambiguity,
    'To what extent was the collapse of the gold standard truly ''overdetermined'' and ''inevitable,'' versus being the result of specific policy choices made by key actors?',
    'Counterfactual historical analysis: detailed modeling of alternative policy paths under the given structural pressures. Examination of declassified archives to assess the range of options considered by policymakers at the time.',
    'If the collapse was less inevitable, the fiat system''s extractive elements are more attributable to policy choices, potentially reclassifying it closer to a Snare. If truly inevitable, the Tangled Rope classification holds, emphasizing the structural constraints on actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inevitability_vs_choice_ambiguity, conceptual, 'Ambiguity between structural determinism and policy agency in the monetary transition.').

omega_variable(
    long_term_stability_of_fiat_regime,
    'Is the current fiat monetary regime structurally stable in the long term, or does it contain inherent contradictions that will lead to its eventual collapse or transformation?',
    'Ongoing economic performance, particularly regarding inflation, debt levels, and financial crises. Comparative analysis with historical monetary regimes and their lifecycles.',
    'If the fiat regime proves unstable, its classification might drift towards Piton (atrophied function) or even Snare (unsustainable extraction). If it demonstrates long-term adaptability, its Rope-like coordination aspects would be reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_stability_of_fiat_regime, empirical, 'Uncertainty about the long-term viability of the fiat monetary system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__overdetermined_composite_reading, 1968, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1968, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1968, 0.15).
narrative_ontology:measurement(mone_tr_t1978, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1978, 0.12).
narrative_ontology:measurement(mone_tr_t1988, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1988, 0.1).
narrative_ontology:measurement(mone_tr_t1998, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1998, 0.09).
narrative_ontology:measurement(mone_tr_t2008, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 2008, 0.09).
narrative_ontology:measurement(mone_tr_t2023, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(mone_be_t1968, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1968, 0.6).
narrative_ontology:measurement(mone_be_t1978, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1978, 0.7).
narrative_ontology:measurement(mone_be_t1988, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1988, 0.75).
narrative_ontology:measurement(mone_be_t1998, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1998, 0.8).
narrative_ontology:measurement(mone_be_t2008, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 2008, 0.83).
narrative_ontology:measurement(mone_be_t2023, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 2023, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t1968, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1968, 0.55).
narrative_ontology:measurement(mone_su_t1978, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1978, 0.65).
narrative_ontology:measurement(mone_su_t1988, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1988, 0.7).
narrative_ontology:measurement(mone_su_t1998, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1998, 0.72).
narrative_ontology:measurement(mone_su_t2008, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 2008, 0.74).
narrative_ontology:measurement(mone_su_t2023, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 2023, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__overdetermined_composite_reading, resource_allocation).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, triffin_inevitability_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'monetary_anchor_principle' kernel. This 'overdetermined composite' reading emphasizes the multi-causal inevitability of the gold standard's collapse and the emergence of the fiat system, contrasting with readings that focus on single policy choices or single structural causes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
