% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__punctuated_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__punctuated_swap_reading, []).

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
 *   constraint_id: monetary_anchor_principle__punctuated_swap_reading
 *   human_readable: Monetary Anchor Principle: Punctuated Swap Reading (1971 Nixon Shock)
 *   domain: Monetary Economics / Political Economy / International Finance
 *
 * SUMMARY:
 *   This constraint story analyzes the 'monetary anchor principle' through
 *   the lens of the 'punctuated swap' reading, which posits that the August
 *   15, 1971 decision by the US to unilaterally suspend the dollar's
 *   convertibility to gold was a discrete institutional choice, fundamentally
 *   altering the international monetary regime. This reading emphasizes the
 *   agency of the US government in initiating a shift from one system
 *   (Bretton Woods) to another (floating exchange rates), rather than viewing
 *   it as an inevitable outcome of structural forces.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, 0.55).
domain_priors:suppression_score(monetary_anchor_principle__punctuated_swap_reading, 0.7).
domain_priors:theater_ratio(monetary_anchor_principle__punctuated_swap_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__punctuated_swap_reading, rope).
narrative_ontology:human_readable(monetary_anchor_principle__punctuated_swap_reading, "Monetary Anchor Principle: Punctuated Swap Reading (1971 Nixon Shock)").
narrative_ontology:topic_domain(monetary_anchor_principle__punctuated_swap_reading, "Monetary Economics / Political Economy / International Finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__punctuated_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__punctuated_swap_reading, '1eb58075-6649-4e6e-b09e-d41f0e300049').
narrative_ontology:cs_kernel_codification('1eb58075-6649-4e6e-b09e-d41f0e300049', formalized).
narrative_ontology:cs_authority_grounding('1eb58075-6649-4e6e-b09e-d41f0e300049', lineage).
narrative_ontology:cs_interpretation_layer_present('1eb58075-6649-4e6e-b09e-d41f0e300049').
narrative_ontology:cs_reading_relation('1eb58075-6649-4e6e-b09e-d41f0e300049', monetary_anchor_principle__overdetermined_composite_reading, forecloses).
narrative_ontology:cs_reading_relation('1eb58075-6649-4e6e-b09e-d41f0e300049', monetary_anchor_principle__triffin_inevitability_reading, forecloses).
narrative_ontology:cs_axiom('1eb58075-6649-4e6e-b09e-d41f0e300049', foundational, institutional_choice_primacy).
narrative_ontology:cs_axiom_status(institutional_choice_primacy, holdable).
narrative_ontology:cs_axiom_grounding('1eb58075-6649-4e6e-b09e-d41f0e300049', institutional_choice_primacy, conventional).
narrative_ontology:cs_axiom('1eb58075-6649-4e6e-b09e-d41f0e300049', foundational, national_sovereignty_in_monetary_policy).
narrative_ontology:cs_axiom_status(national_sovereignty_in_monetary_policy, holdable).
narrative_ontology:cs_axiom_grounding('1eb58075-6649-4e6e-b09e-d41f0e300049', national_sovereignty_in_monetary_policy, conventional).
narrative_ontology:cs_reference_frame('1eb58075-6649-4e6e-b09e-d41f0e300049', bretton_woods_fixed_exchange_rate_system).
narrative_ontology:cs_drift_state('1eb58075-6649-4e6e-b09e-d41f0e300049', nixon_shock_1971, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('1eb58075-6649-4e6e-b09e-d41f0e300049', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_government).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_treasury).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, federal_reserve).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, foreign_central_banks).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, foreign_governments).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, international_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the sovereign power, it made the unilateral decision to suspend dollar convertibility to gold, gaining immediate fiscal and monetary policy autonomy. It directly benefited from the ability to devalue the dollar and manage domestic economic pressures without external constraint.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Responsible for implementing the new monetary policy and managing the transition away from the fixed exchange rate system. It gained flexibility in managing national debt and financing government expenditures.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_treasury, agenda_setter,
    institutional, biographical, arbitrage, national).

% Supported the government's decision and gained greater freedom in conducting domestic monetary policy, no longer constrained by the need to defend the dollar's gold parity. This allowed for more active management of inflation and employment.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, federal_reserve, agenda_setter,
    institutional, biographical, arbitrage, national).

% Held significant dollar reserves, which immediately lost value due to the suspension of convertibility and subsequent devaluation. They faced instability in their own currencies and lost a key mechanism for international monetary adjustment.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, foreign_central_banks, payer,
    institutional, biographical, constrained, global).

% Faced economic disruption, increased import costs, and reduced purchasing power for their dollar-denominated assets. They lost leverage over US economic policy and were forced to adapt to a new, more volatile international monetary environment.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, foreign_governments, payer,
    institutional, biographical, constrained, global).

% Experienced immediate uncertainty and volatility in currency markets. Those holding dollar-denominated assets saw their value diminish, leading to capital losses and a scramble to reallocate portfolios.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, international_investors, payer,
    powerful, immediate, mobile, global).

% Its foundational principles of fixed exchange rates and multilateral adjustment were undermined by the unilateral US action. While it continued to exist, its role in managing the international monetary system was significantly diminished, and it lacked the power to enforce the prior regime.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, international_monetary_fund, excluded,
    institutional, generational, constrained, global).

% Analyze the causes, consequences, and alternative interpretations of the 1971 decision, contributing to the ongoing academic debate about its nature and impact.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, economic_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monetary_anchor_principle__punctuated_swap_reading, us_government).
narrative_ontology:fixing_cost_class(monetary_anchor_principle__punctuated_swap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Bretton Woods system, which the 1971 decision ended, aimed to coordinate international exchange rates and provide global monetary stability through a dollar-gold peg. The 'punctuated swap' was a unilateral defection from this coordination.
% TRANSFER_FUNCTION: The constraint transferred the costs of US fiscal and monetary policy (e.g., Vietnam War deficits, domestic inflation) from the US to foreign dollar holders, primarily through the devaluation of their dollar reserves and subsequent inflation.
% ABSENT_VOICES: The international community, particularly foreign central banks and governments, whose concerns about dollar convertibility were unilaterally overridden. The institutional mandate of the IMF was sidelined, and its voice in managing the transition was largely ignored.
% DISAPPEARANCE_RATIONALE: If the August 15, 1971 decision had not occurred, the international monetary system would likely still be grappling with the Bretton Woods constraints or have transitioned differently. The current global financial architecture, characterized by floating exchange rates and increased capital mobility, is a direct consequence of this 'punctuated swap.'
% FOUNDING_PROBLEM: The US faced a growing balance of payments deficit and a declining gold reserve, making it increasingly difficult to maintain the dollar's convertibility to gold at the fixed rate of $35 per ounce, as mandated by the Bretton Woods agreement.
% FOUNDING_PROBLEM_CORROBORATION: US government statements at the time, historical records, and economic analyses from various international bodies and academics corroborate the immediate pressures on the US. While the specific gold-convertibility problem is dead, the underlying tension between national fiscal autonomy and international monetary stability persists, making the broader 'founding problem' still live.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__punctuated_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__punctuated_swap_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__punctuated_swap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(monetary_anchor_principle__punctuated_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__punctuated_swap_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__punctuated_swap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(monetary_anchor_principle__punctuated_swap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is 'rope' because the Bretton Woods system was initially a coordination mechanism. However, the metrics reflect the post-defection reality: extractiveness is moderate (0.55) due to the wealth transfer from foreign dollar holders to the US via devaluation. Suppression is high (0.7) because the unilateral action effectively suppressed alternatives for foreign central banks and governments, forcing them to accept the new regime. Theater ratio is low (0.1) as the action was a direct, functional change, not performative maintenance. Accessibility collapse is high (0.8) for foreign holders, as their ability to demand gold for dollars vanished. Resistance was significant (0.75) from affected nations. The measurement series track the stabilization of the new regime and its ongoing extractive and suppressive effects over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the US agenda-setters, the decision was a necessary act of national sovereignty to address domestic economic pressures and a failing international system. From the perspective of foreign dollar holders, it was an act of expropriation and a breach of international trust. The engine's per-seat classification will highlight this divergence, showing a 'rope' (or even 'snare') for victims and a 'beneficiary' or 'arbitrage' position for the US.
 *
 * DIRECTIONALITY LOGIC:
 *   The US government, Treasury, and Federal Reserve are clear beneficiaries, gaining fiscal and monetary autonomy. Foreign central banks, foreign governments, and international investors are victims, bearing the costs of devaluation and increased volatility. The IMF is an excluded party, its institutional mandate undermined by the unilateral action. This structural asymmetry drives the engine's directionality computation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causality_of_nixon_shock,
    'Was the August 15, 1971 decision a discrete institutional choice, or the inevitable outcome of structural pressures (e.g., Triffin dilemma, Vietnam War deficits)?',
    'Historical counterfactual analysis, re-evaluation of primary sources, and economic modeling of alternative policy paths available to the US at the time. This would involve assessing the degree of policy discretion versus systemic determinism.',
    'If found to be inevitable, this reading''s emphasis on choice would be undermined, potentially shifting the constraint''s classification towards a Mountain (if truly natural law) or a Piton (if the inevitability was merely theatrical maintenance of a failing system). If confirmed as a choice, it reinforces the Rope classification and the agency of the US.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causality_of_nixon_shock, conceptual, 'Ambiguity regarding the causal nature of the 1971 monetary transition.').

omega_variable(
    extent_of_expropriation,
    'What was the precise magnitude of wealth transfer from foreign dollar holders to the US due to the devaluation and subsequent inflation, and how much of this was ''necessary adjustment'' versus ''unilateral extraction''?',
    'Detailed econometric analysis of exchange rate movements, inflation differentials, and foreign reserve holdings post-1971, coupled with a normative framework for distinguishing legitimate adjustment from extractive transfer.',
    'A higher quantified transfer would strengthen the ''victim'' declaration and push extractiveness higher, potentially reclassifying the constraint towards a Snare. A lower transfer would support a more balanced ''Rope'' interpretation, emphasizing the coordination problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extent_of_expropriation, empirical, 'Quantification of the wealth transfer and its characterization as adjustment or extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__punctuated_swap_reading, 1971, 2021).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1971, 0.1).
narrative_ontology:measurement(mone_tr_t1981, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1981, 0.1).
narrative_ontology:measurement(mone_tr_t1991, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1991, 0.1).
narrative_ontology:measurement(mone_tr_t2001, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 2001, 0.1).
narrative_ontology:measurement(mone_tr_t2011, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 2011, 0.1).
narrative_ontology:measurement(mone_tr_t2021, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 2021, 0.1).

% Extraction over time
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1971, 0.5).
narrative_ontology:measurement(mone_be_t1981, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1981, 0.52).
narrative_ontology:measurement(mone_be_t1991, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1991, 0.53).
narrative_ontology:measurement(mone_be_t2001, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 2001, 0.54).
narrative_ontology:measurement(mone_be_t2011, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 2011, 0.55).
narrative_ontology:measurement(mone_be_t2021, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 2021, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t1971, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1971, 0.6).
narrative_ontology:measurement(mone_su_t1981, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1981, 0.65).
narrative_ontology:measurement(mone_su_t1991, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1991, 0.68).
narrative_ontology:measurement(mone_su_t2001, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 2001, 0.7).
narrative_ontology:measurement(mone_su_t2011, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 2011, 0.7).
narrative_ontology:measurement(mone_su_t2021, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 2021, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__punctuated_swap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, floating_exchange_rate_regime).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, petrodollar_system).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__overdetermined_composite_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__triffin_inevitability_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'monetary_anchor_principle' kernel. This reading emphasizes the 1971 Nixon Shock as a discrete institutional choice, distinct from readings that emphasize structural inevitability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
