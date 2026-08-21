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
 *   constraint_id: transition_causality__hybrid_trigger_reading
 *   human_readable: Bretton Woods Collapse: Hybrid Trigger Reading
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story represents the 'hybrid trigger' reading of the
 *   Bretton Woods collapse. It posits that while structural contradictions
 *   (like the Triffin Dilemma) made the system inherently unstable, its
 *   actual demise required specific contingent events, such as the fiscal
 *   shock of the Vietnam War and the French gold runs, to act as triggers.
 *   Without these triggers, the system might have persisted longer or
 *   collapsed differently. The claimed type is 'tangled_rope' because it
 *   provided genuine coordination (post-war stability) but also enabled
 *   asymmetric extraction by the US, requiring active enforcement to maintain
 *   dollar convertibility.
 *
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
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__hybrid_trigger_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__hybrid_trigger_reading, "Bretton Woods Collapse: Hybrid Trigger Reading").
narrative_ontology:topic_domain(transition_causality__hybrid_trigger_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(transition_causality__hybrid_trigger_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__hybrid_trigger_reading, '51060697-846c-4077-8cdb-7c911439c49d').
narrative_ontology:cs_kernel_codification('51060697-846c-4077-8cdb-7c911439c49d', formalized).
narrative_ontology:cs_authority_grounding('51060697-846c-4077-8cdb-7c911439c49d', lineage).
narrative_ontology:cs_interpretation_layer_present('51060697-846c-4077-8cdb-7c911439c49d').
narrative_ontology:cs_reading_relation('51060697-846c-4077-8cdb-7c911439c49d', transition_causality__contingent_choice_reading, coexists_with).
narrative_ontology:cs_reading_relation('51060697-846c-4077-8cdb-7c911439c49d', transition_causality__overdetermined_collapse_reading, coexists_with).
narrative_ontology:cs_axiom('51060697-846c-4077-8cdb-7c911439c49d', foundational, structural_contradictions_create_instability).
narrative_ontology:cs_axiom_status(structural_contradictions_create_instability, holdable).
narrative_ontology:cs_axiom_grounding('51060697-846c-4077-8cdb-7c911439c49d', structural_contradictions_create_instability, empirically_contingent).
narrative_ontology:cs_axiom('51060697-846c-4077-8cdb-7c911439c49d', foundational, contingent_events_actualize_collapse).
narrative_ontology:cs_axiom_status(contingent_events_actualize_collapse, holdable).
narrative_ontology:cs_axiom_grounding('51060697-846c-4077-8cdb-7c911439c49d', contingent_events_actualize_collapse, empirically_contingent).
narrative_ontology:cs_reference_frame('51060697-846c-4077-8cdb-7c911439c49d', bretton_woods_gold_dollar_standard).
narrative_ontology:cs_drift_state('51060697-846c-4077-8cdb-7c911439c49d', post_vietnam_war_fiscal_shock, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('51060697-846c-4077-8cdb-7c911439c49d', '').
narrative_ontology:cs_kernel_id(transition_causality__hybrid_trigger_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, us_hegemon).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, international_financial_institutions).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, european_central_banks).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, developing_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefited from the dollar's reserve currency status, allowing it to run persistent balance of payments deficits. Faced increasing pressure as foreign dollar holdings grew, but resisted revaluation or gold convertibility changes until forced. Its policy choices (e.g., Vietnam War financing) acted as contingent triggers.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, us_hegemon, agenda_setter,
    institutional, generational, constrained, global).

% Accumulated large dollar reserves, which were increasingly seen as overvalued relative to gold. Their attempts to convert dollars to gold (e.g., France) put direct pressure on the US gold reserves, acting as a trigger for the collapse. Their options were limited by the system's design and the dollar's central role.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, european_central_banks, payer,
    organized, biographical, constrained, regional).

% Administered the Bretton Woods system, benefiting from its stability and the US dollar's central role. While not directly extracting, their institutional mandate was to maintain the system, which meant defending the existing structure even as contradictions mounted. They adapted to the post-Bretton Woods era.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, international_financial_institutions, beneficiary,
    institutional, generational, mobile, global).

% Had little influence over the system's design or its eventual collapse. They bore the costs of currency instability and the dollar's fluctuations, often exacerbating their debt burdens and development challenges. Their options were severely limited.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, developing_nations, payer,
    powerless, generational, trapped, global).

% Analyzed the structural contradictions (e.g., Triffin Dilemma) and the contingent triggers of the collapse. Their work informs the understanding of the system's inherent instability and the role of specific events in its demise.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, academic_economists, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a stable international monetary system with fixed exchange rates, anchored by the US dollar convertible to gold, facilitating post-war reconstruction and global trade.
% TRANSFER_FUNCTION: Transferred seigniorage benefits and policy autonomy to the US (as issuer of the reserve currency) from other nations, who bore the risk of dollar overvaluation and limited monetary policy independence.
% ABSENT_VOICES: Many developing nations, whose economic stability was heavily impacted by the system's design and collapse, had minimal voice in its governance or the decisions leading to its end. Their perspectives on the system's inherent unfairness were largely unheard.
% DISAPPEARANCE_RATIONALE: The collapse of Bretton Woods led to a fundamental shift in the international monetary system, moving to floating exchange rates and a more complex, multi-polar financial landscape. Global trade, capital flows, and national monetary policies were profoundly reorganized.
% FOUNDING_PROBLEM: The interwar period was characterized by competitive devaluations, currency instability, and trade protectionism, which contributed to economic depression and international conflict. Bretton Woods aimed to prevent a return to this chaos.
% FOUNDING_PROBLEM_CORROBORATION: While the problem of currency instability is perennial, the specific conditions of the interwar period (e.g., gold standard rigidity, lack of international cooperation) are largely gone. Most economists and historians outside the US hegemon's direct influence agree the original problem was solved, but the system's design created new, unsustainable contradictions.
narrative_ontology:disappearance_verdict(transition_causality__hybrid_trigger_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__hybrid_trigger_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__hybrid_trigger_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness increased over time as the US exploited its 'exorbitant privilege' to run deficits, effectively extracting resources from other nations holding dollars. Suppression was high because the system's rules and the US's economic power limited alternatives for other nations. Theater ratio rose as the US maintained the pretense of gold convertibility even as its gold reserves dwindled relative to dollar liabilities. The system required active enforcement to prevent widespread gold conversions and maintain the dollar's central role.
 *
 * PERSPECTIVAL GAP:
 *   The US hegemon would likely view the system as a necessary coordination mechanism that eventually became unsustainable due to external pressures. European central banks would see it as increasingly extractive, forcing them to hold depreciating assets. This reading acknowledges both the structural pressures and the contingent events that actualized the collapse, bridging the gap between purely structural and purely volitional accounts.
 *
 * DIRECTIONALITY LOGIC:
 *   The US hegemon was the primary beneficiary, gaining seigniorage and policy flexibility. European central banks and developing nations were payers, bearing the costs of dollar overvaluation and limited monetary autonomy. International financial institutions benefited from the system's stability and their role in its administration. Academic economists served as observers, analyzing the system's dynamics.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_trigger_timing,
    'How much longer could the Bretton Woods system have persisted, and in what form, if the specific contingent triggers (Vietnam War, French gold runs) had not occurred when they did?',
    'Detailed counterfactual historical analysis and agent-based modeling exploring alternative policy paths and geopolitical events.',
    'A high counterfactual viability would strengthen the ''contingent choice'' reading, suggesting the system was less structurally determined. Low viability would strengthen the ''overdetermined collapse'' reading, suggesting other triggers would have emerged quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_trigger_timing, empirical, 'Assessing the counterfactual viability of Bretton Woods without specific triggers.').

omega_variable(
    triffin_dilemma_inevitability,
    'Was the Triffin Dilemma (the inherent contradiction of a national currency serving as an international reserve asset) truly an unavoidable structural contradiction, or could it have been managed indefinitely with different policy choices?',
    'Comparative analysis with other reserve currency systems and theoretical modeling of alternative international monetary architectures.',
    'If unavoidable, it strengthens the structural component of this hybrid reading. If manageable, it shifts weight towards the ''contingent choice'' reading, suggesting the structural contradiction was not a hard limit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(triffin_dilemma_inevitability, conceptual, 'The degree of inevitability of the Triffin Dilemma''s consequences.').

omega_variable(
    trigger_event_definition,
    'What constitutes a ''contingent trigger event'' versus a symptom of underlying structural stress? Is the distinction between structural and contingent clear enough to be analytically useful?',
    'Refinement of causal definitions in historical and economic analysis, focusing on events with high counterfactual leverage.',
    'A clearer definition strengthens the analytical precision of the hybrid reading. Ambiguity weakens it, potentially blurring the lines with ''overdetermined collapse'' or ''contingent choice'' readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trigger_event_definition, conceptual, 'Clarity of distinction between structural stress and contingent trigger events.').


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
narrative_ontology:measurement(tran_tr_t1965, transition_causality__hybrid_trigger_reading, theater_ratio, 1965, 0.3).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__hybrid_trigger_reading, theater_ratio, 1971, 0.4).

% Extraction over time
narrative_ontology:measurement(tran_be_t1944, transition_causality__hybrid_trigger_reading, base_extractiveness, 1944, 0.4).
narrative_ontology:measurement(tran_be_t1955, transition_causality__hybrid_trigger_reading, base_extractiveness, 1955, 0.5).
narrative_ontology:measurement(tran_be_t1965, transition_causality__hybrid_trigger_reading, base_extractiveness, 1965, 0.6).
narrative_ontology:measurement(tran_be_t1971, transition_causality__hybrid_trigger_reading, base_extractiveness, 1971, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1944, transition_causality__hybrid_trigger_reading, suppression_requirement, 1944, 0.5).
narrative_ontology:measurement(tran_su_t1955, transition_causality__hybrid_trigger_reading, suppression_requirement, 1955, 0.58).
narrative_ontology:measurement(tran_su_t1965, transition_causality__hybrid_trigger_reading, suppression_requirement, 1965, 0.65).
narrative_ontology:measurement(tran_su_t1971, transition_causality__hybrid_trigger_reading, suppression_requirement, 1971, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__hybrid_trigger_reading, global_infrastructure).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, post_bretton_woods_floating_rates).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, petrodollar_system).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'transition_causality' kernel, focusing on the hybrid role of structural contradictions and contingent triggers in the Bretton Woods collapse. It links to the post-Bretton Woods monetary order.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
