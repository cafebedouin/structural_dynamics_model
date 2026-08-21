% ============================================================================
% CONSTRAINT STORY: transition_causality__hybrid_trigger_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__hybrid_trigger_reading, []).

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
 *   constraint_id: transition_causality__hybrid_trigger_reading
 *   human_readable: Bretton Woods Collapse: Hybrid Trigger Reading
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story analyzes the collapse of the Bretton Woods system
 *   through the 'hybrid trigger' reading of transition causality. It posits
 *   that while structural contradictions (like the Triffin Dilemma)
 *   accumulated over decades, specific contingent events—such as the fiscal
 *   shock of the Vietnam War and the French gold runs—were necessary triggers
 *   that actualized the system's collapse. The system, initially a genuine
 *   coordination mechanism, became increasingly extractive as the US
 *   leveraged its reserve currency status, requiring active enforcement to
 *   maintain convertibility promises that were no longer credible.
 *
 * KEY AGENTS:
 *   - united_states_treasury: Agenda setter (institutional/constrained) — benefited from the system but faced increasing pressure.
 *   - european_central_banks: Payer (organized/constrained) — bore costs of dollar overvaluation, but constrained by stability concerns.
 *   - developing_nations: Payer (powerless/trapped) — bore costs of instability with no influence.
 *   - international_monetary_fund: Observer (institutional/analytical) — identified problems but lacked enforcement power.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, 0.65).
domain_priors:suppression_score(transition_causality__hybrid_trigger_reading, 0.7).
domain_priors:theater_ratio(transition_causality__hybrid_trigger_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__hybrid_trigger_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__hybrid_trigger_reading, "Bretton Woods Collapse: Hybrid Trigger Reading").
narrative_ontology:topic_domain(transition_causality__hybrid_trigger_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(transition_causality__hybrid_trigger_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__hybrid_trigger_reading, '088d3a60-fe42-47fd-9a79-d45a604ab9a6').
narrative_ontology:cs_kernel_codification('088d3a60-fe42-47fd-9a79-d45a604ab9a6', formalized).
narrative_ontology:cs_authority_grounding('088d3a60-fe42-47fd-9a79-d45a604ab9a6', lineage).
narrative_ontology:cs_interpretation_layer_present('088d3a60-fe42-47fd-9a79-d45a604ab9a6').
narrative_ontology:cs_reading_relation('088d3a60-fe42-47fd-9a79-d45a604ab9a6', transition_causality__contingent_choice_reading, coexists_with).
narrative_ontology:cs_reading_relation('088d3a60-fe42-47fd-9a79-d45a604ab9a6', transition_causality__overdetermined_collapse_reading, coexists_with).
narrative_ontology:cs_axiom('088d3a60-fe42-47fd-9a79-d45a604ab9a6', foundational, structural_contradictions_create_preconditions).
narrative_ontology:cs_axiom_status(structural_contradictions_create_preconditions, holdable).
narrative_ontology:cs_axiom_grounding('088d3a60-fe42-47fd-9a79-d45a604ab9a6', structural_contradictions_create_preconditions, empirically_contingent).
narrative_ontology:cs_axiom('088d3a60-fe42-47fd-9a79-d45a604ab9a6', foundational, contingent_events_actualize_collapse).
narrative_ontology:cs_axiom_status(contingent_events_actualize_collapse, holdable).
narrative_ontology:cs_axiom_grounding('088d3a60-fe42-47fd-9a79-d45a604ab9a6', contingent_events_actualize_collapse, empirically_contingent).
narrative_ontology:cs_reference_frame('088d3a60-fe42-47fd-9a79-d45a604ab9a6', bretton_woods_original_design).
narrative_ontology:cs_drift_state('088d3a60-fe42-47fd-9a79-d45a604ab9a6', post_vietnam_war_fiscal_shock, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('088d3a60-fe42-47fd-9a79-d45a604ab9a6', '').
narrative_ontology:cs_kernel_id(transition_causality__hybrid_trigger_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, united_states_treasury).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, us_hegemonic_power).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, european_central_banks).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, developing_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administered the Bretton Woods system, benefiting from the dollar's reserve currency status which allowed it to run persistent balance of payments deficits. Faced increasing pressure from gold outflows but resisted revaluation or convertibility changes until forced.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, united_states_treasury, agenda_setter,
    institutional, generational, constrained, global).

% Accumulated large dollar reserves, increasingly concerned about the dollar's overvaluation and the declining gold cover. Their attempts to convert dollars to gold put pressure on the US, but they were constrained by the desire to maintain international monetary stability.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, european_central_banks, payer,
    organized, biographical, constrained, continental).

% Had little influence over the system's design or its collapse. They bore the costs of dollar instability and the subsequent shift to floating exchange rates, often experiencing capital flight and increased debt burdens.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, developing_nations, payer,
    powerless, generational, trapped, global).

% The institutional body designed to oversee the Bretton Woods system. Its analyses highlighted the Triffin Dilemma but it lacked the power to compel the necessary policy changes from member states, particularly the US.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a stable international monetary system based on fixed exchange rates pegged to the dollar, which was convertible to gold, facilitating post-war reconstruction and trade.
% TRANSFER_FUNCTION: Transferred seigniorage benefits and the ability to run persistent deficits to the US, while other nations provided real goods and services in exchange for dollars that were increasingly difficult to convert to gold.
% ABSENT_VOICES: Economists advocating for a more flexible international monetary system or a truly multilateral reserve asset were largely excluded from the core decision-making bodies, their warnings about the Triffin Dilemma often unheeded until crisis.
% DISAPPEARANCE_RATIONALE: The collapse of Bretton Woods led to a fundamental shift in international finance, moving to floating exchange rates and a more volatile global monetary system. The world rearranged around new mechanisms for managing currency risk and capital flows.
% FOUNDING_PROBLEM: The instability of the interwar period, characterized by competitive devaluations and trade wars, necessitated a new international monetary order to promote stability and facilitate global trade and investment.
% FOUNDING_PROBLEM_CORROBORATION: While the need for international monetary stability remains, the specific problems Bretton Woods was designed to solve (e.g., competitive devaluations under a gold standard) had largely been superseded by new challenges. Independent economic historians and international relations scholars widely corroborate that the system's original mandate was no longer fully aligned with global economic realities by the late 1960s.
narrative_ontology:disappearance_verdict(transition_causality__hybrid_trigger_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__hybrid_trigger_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__hybrid_trigger_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(transition_causality__hybrid_trigger_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__hybrid_trigger_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__hybrid_trigger_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(transition_causality__hybrid_trigger_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness increased over time as the US exploited the 'exorbitant privilege' of the dollar's reserve status, running deficits without immediate consequence, effectively extracting real resources from other nations. Suppression was high because the system required other nations to hold dollars and not demand gold, enforced by diplomatic pressure and the lack of viable alternatives. The theater ratio rose as the US maintained the 'gold convertibility' facade even as its gold reserves dwindled relative to dollar liabilities. The claimed type is 'tangled_rope' because it had a genuine coordination function (post-war stability) but developed significant asymmetric extraction, requiring active enforcement to persist.
 *
 * PERSPECTIVAL GAP:
 *   From the US Treasury's perspective, the system was a necessary global public good that required US leadership and sacrifice, justifying its benefits. From European central banks, it was an increasingly unfair burden. Developing nations experienced it as a distant, imposed structure. The engine's per-seat classification would reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   The US Treasury is a clear beneficiary (d near 0.0) due to seigniorage and deficit financing. European central banks and developing nations are payers (d near 1.0) as they bore the costs of dollar overvaluation and instability. The IMF is an analytical observer (d near 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope, with rising extractiveness and suppression, prevents mislabeling the Bretton Woods system as a pure Rope (coordination) or a pure Snare (extraction). It acknowledges its initial coordination function while highlighting its drift towards extraction, which eventually led to its collapse when contingent triggers exposed the accumulated contradictions. The 'dead' status of the founding problem, combined with the 'world_rearranges' verdict, signals a zombie-like persistence of the extractive elements even after the original coordination need diminished.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_trigger_timing,
    'How viable was the Bretton Woods system if the contingent triggers (Vietnam War, French gold runs) had not occurred or been delayed?',
    'Detailed counterfactual historical analysis and agent-based modeling exploring alternative policy responses and external shocks.',
    'If the system had high counterfactual viability, it would strengthen the ''contingent_choice_reading'' by suggesting policy could have averted collapse. If low, it would lean towards the ''overdetermined_collapse_reading''. This reading assumes medium viability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_trigger_timing, empirical, 'Assesses the sensitivity of the system''s collapse to specific trigger events.').

omega_variable(
    triffin_dilemma_inevitability,
    'Was the Triffin Dilemma an inevitable structural contradiction that guaranteed collapse, or could it have been managed indefinitely with different policy choices?',
    'Comparative analysis with other reserve currency systems and theoretical modeling of international monetary reform proposals (e.g., SDR expansion).',
    'If inevitable, it strengthens the ''overdetermined_collapse_reading''. If manageable, it supports the ''contingent_choice_reading''. This reading views it as a slow-burning contradiction that needed a trigger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(triffin_dilemma_inevitability, conceptual, 'Examines the inherent stability of the dollar-gold peg.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__hybrid_trigger_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1944, transition_causality__hybrid_trigger_reading, theater_ratio, 1944, 0.1).
narrative_ontology:measurement(tran_tr_t1955, transition_causality__hybrid_trigger_reading, theater_ratio, 1955, 0.2).
narrative_ontology:measurement(tran_tr_t1965, transition_causality__hybrid_trigger_reading, theater_ratio, 1965, 0.35).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__hybrid_trigger_reading, theater_ratio, 1971, 0.4).

% Extraction over time
narrative_ontology:measurement(tran_be_t1944, transition_causality__hybrid_trigger_reading, base_extractiveness, 1944, 0.3).
narrative_ontology:measurement(tran_be_t1955, transition_causality__hybrid_trigger_reading, base_extractiveness, 1955, 0.45).
narrative_ontology:measurement(tran_be_t1965, transition_causality__hybrid_trigger_reading, base_extractiveness, 1965, 0.6).
narrative_ontology:measurement(tran_be_t1971, transition_causality__hybrid_trigger_reading, base_extractiveness, 1971, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1944, transition_causality__hybrid_trigger_reading, suppression_requirement, 1944, 0.4).
narrative_ontology:measurement(tran_su_t1955, transition_causality__hybrid_trigger_reading, suppression_requirement, 1955, 0.55).
narrative_ontology:measurement(tran_su_t1965, transition_causality__hybrid_trigger_reading, suppression_requirement, 1965, 0.65).
narrative_ontology:measurement(tran_su_t1971, transition_causality__hybrid_trigger_reading, suppression_requirement, 1971, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__hybrid_trigger_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'transition_causality' kernel, focusing on the hybrid role of structural contradictions and contingent triggers in the Bretton Woods collapse. It is linked to 'contingent_choice_reading' and 'overdetermined_collapse_reading' as sibling interpretations of the same historical event.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
