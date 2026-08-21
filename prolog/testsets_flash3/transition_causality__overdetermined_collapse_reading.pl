% ============================================================================
% CONSTRAINT STORY: transition_causality__overdetermined_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__overdetermined_collapse_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: transition_causality__overdetermined_collapse_reading
 *   human_readable: Overdetermined Collapse of Fixed Exchange Rate Regime
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'overdetermined collapse' reading
 *   of the transition causality kernel. It posits that the collapse of the
 *   fixed exchange rate regime was structurally inevitable, driven by
 *   reinforcing contradictions such as the Triffin Dilemma. Policy choices
 *   and contingent events are viewed as epiphenomenal to the underlying
 *   structural forces. The constraint is classified as a Mountain because its
 *   inevitability is presented as an irreducible feature of the system, not a
 *   human choice, from this reading's perspective.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, 0.9).
domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, 0.95).
domain_priors:theater_ratio(transition_causality__overdetermined_collapse_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__overdetermined_collapse_reading, mountain).
narrative_ontology:human_readable(transition_causality__overdetermined_collapse_reading, "Overdetermined Collapse of Fixed Exchange Rate Regime").
narrative_ontology:topic_domain(transition_causality__overdetermined_collapse_reading, "monetary_economics/political_economy/international_finance").

domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__overdetermined_collapse_reading, '0b6782d5-1a6a-422e-ae13-d10a3139006f').
narrative_ontology:cs_kernel_codification('0b6782d5-1a6a-422e-ae13-d10a3139006f', implicit).
narrative_ontology:cs_authority_grounding('0b6782d5-1a6a-422e-ae13-d10a3139006f', diffuse_epistemic).
narrative_ontology:cs_reading_relation('0b6782d5-1a6a-422e-ae13-d10a3139006f', transition_causality__contingent_choice_reading, forecloses).
narrative_ontology:cs_reading_relation('0b6782d5-1a6a-422e-ae13-d10a3139006f', transition_causality__hybrid_trigger_reading, influences).
narrative_ontology:cs_axiom('0b6782d5-1a6a-422e-ae13-d10a3139006f', foundational, structural_contradictions_are_determinative).
narrative_ontology:cs_axiom_status(structural_contradictions_are_determinative, holdable).
narrative_ontology:cs_axiom_grounding('0b6782d5-1a6a-422e-ae13-d10a3139006f', structural_contradictions_are_determinative, empirically_contingent).
narrative_ontology:cs_axiom('0b6782d5-1a6a-422e-ae13-d10a3139006f', foundational, triffin_dilemma_is_an_inherent_system_flaw).
narrative_ontology:cs_axiom_status(triffin_dilemma_is_an_inherent_system_flaw, holdable).
narrative_ontology:cs_axiom_grounding('0b6782d5-1a6a-422e-ae13-d10a3139006f', triffin_dilemma_is_an_inherent_system_flaw, empirically_contingent).
narrative_ontology:cs_reference_frame('0b6782d5-1a6a-422e-ae13-d10a3139006f', fixed_exchange_rate_system_inherent_instability).
narrative_ontology:cs_drift_state('0b6782d5-1a6a-422e-ae13-d10a3139006f', post_bretton_woods_collapse, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('0b6782d5-1a6a-422e-ae13-d10a3139006f', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(transition_causality__overdetermined_collapse_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, analytical_observers).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, fixed_exchange_rate_regime_participants).
narrative_ontology:constraint_vindicates(transition_causality__overdetermined_collapse_reading, triffin_dilemma_theory).
narrative_ontology:constraint_vindicates(transition_causality__overdetermined_collapse_reading, structural_determinism_in_macroeconomics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All actors (nations, central banks, corporations) operating under the fixed exchange rate system were structurally constrained by its inherent contradictions, leading to an inevitable collapse regardless of individual policy choices. They bore the costs of the system's instability and eventual breakdown.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, fixed_exchange_rate_regime_participants, payer,
    institutional, generational, trapped, global).

% Those who analyze the system from a structural perspective benefit from the vindication of theories like the Triffin Dilemma, which predicted the collapse. Their understanding of economic history is reinforced by the inevitability of the transition.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, analytical_observers, beneficiary,
    analytical, civilizational, analytical, universal).

% Advocates for the 'contingent choice' reading, who believe the transition could have been avoided by different policy decisions, are structurally excluded from this 'overdetermined collapse' reading, as their core premise is denied by the inevitability claim.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, contingent_choice_advocates, excluded,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The fixed exchange rate regime initially coordinated international trade and finance by providing currency stability, but this reading asserts its internal contradictions made its long-term coordination function unsustainable.
% TRANSFER_FUNCTION: The system transferred the costs of its inherent instability and eventual collapse onto all participating economies and financial actors, as they were forced to adapt to the inevitable transition.
% ABSENT_VOICES: Advocates of policy choice and contingent events are absent from this reading's core narrative, as their emphasis on agency is superseded by structural inevitability. They would argue for the significance of specific decisions and events.
% DISAPPEARANCE_RATIONALE: If the 'overdetermined collapse' constraint vanished, the historical fact of the transition would remain, but the interpretation of its inevitability would be open to debate. The world itself would not rearrange, but the understanding of its past would shift, allowing for alternative causal narratives to gain prominence.
% FOUNDING_PROBLEM: The fixed exchange rate regime was established to provide stability for international trade and investment after World War II, preventing competitive devaluations and fostering global economic recovery.
% FOUNDING_PROBLEM_CORROBORATION: Historians and economists widely agree on the initial problem the regime was designed to solve. However, this reading, corroborated by structuralist economists and historical analysis, asserts that the founding problem was superseded by inherent contradictions, rendering the regime's original mandate obsolete long before its actual collapse.
narrative_ontology:disappearance_verdict(transition_causality__overdetermined_collapse_reading, world_unchanged).
narrative_ontology:founding_problem_status(transition_causality__overdetermined_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__overdetermined_collapse_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(transition_causality__overdetermined_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__overdetermined_collapse_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__overdetermined_collapse_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, ExtMetricName, E),
    domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(transition_causality__overdetermined_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.9) and suppression (0.95) reflect the immense costs imposed by the system's inherent instability and the lack of viable alternatives for participants. The accessibility collapse is high (0.9) because this reading asserts no genuine alternatives existed to avert the collapse. Resistance is low (0.05) because, from this perspective, resistance against structural inevitability is futile. The low theater ratio (0.1) indicates that the system's operations were genuinely functional until its inherent contradictions made it impossible to sustain, rather than being performative. The claimed type is Mountain because the reading asserts the transition was a natural, unavoidable outcome of the system's design.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'fixed_exchange_rate_regime_participants', the system was a Snare, extracting costs and trapping them in an unstable structure. From the 'analytical_observers' perspective, it is a Mountain, demonstrating the power of structural forces. The engine's classification will reflect this divergence based on the declared power and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'fixed_exchange_rate_regime_participants' are full targets (d=1.0) as they bore the full costs of the system's inherent instability and eventual collapse. 'Analytical_observers' are beneficiaries (d=0.0) as the system's collapse vindicates their structural theories. The 'contingent_choice_advocates' are excluded, as their perspective is fundamentally incompatible with this reading's core premise of inevitability.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the collapse as a policy failure (Snare) or a coordination problem (Tangled Rope) by emphasizing the structural inevitability. It highlights that the mandate of the fixed exchange rate regime was undermined by its own design, leading to an 'overdetermined collapse' rather than a simple atrophy of function. The 'dead' status of the founding problem, combined with the 'world_unchanged' disappearance verdict, reinforces the idea that the system's end was a structural necessity, not a contingent event.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint truly an ''overdetermined collapse'' (Mountain), or could it be better understood as a ''contingent choice'' (Snare) or ''hybrid trigger'' (Tangled Rope) reading of the transition causality kernel?',
    'Further historical and counterfactual analysis, particularly examining the viability of alternative policy paths and the impact of specific events. If alternative paths are shown to be viable, or specific triggers are found to be necessary, the classification would shift.',
    'If reclassified as ''contingent_choice_reading'', the constraint would likely become a Snare, emphasizing policy-driven extraction. If reclassified as ''hybrid_trigger_reading'', it would likely become a Tangled Rope, acknowledging both structural forces and contingent events.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity in the causal interpretation of the transition.').

omega_variable(
    triffin_dilemma_naturalness,
    'Is the Triffin Dilemma a genuine natural law (Mountain) of international monetary systems, or a constructed constraint (Snare/Tangled Rope) that benefits certain actors?',
    'Comparative analysis of different international monetary systems across history and cultures. If similar dilemmas consistently emerge in structurally analogous systems, it supports naturalness. If its emergence and persistence are tied to specific power structures, it suggests construction.',
    'If the Triffin Dilemma is found to be a constructed constraint, the ''overdetermined collapse'' reading''s claim of inevitability would weaken, potentially shifting its classification towards a Snare or Tangled Rope, as the ''natural'' basis for the collapse would be undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_dilemma_naturalness, empirical, 'The natural-law vs. constructed ambiguity of the Triffin Dilemma.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__overdetermined_collapse_reading, 1944, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1944, transition_causality__overdetermined_collapse_reading, theater_ratio, 1944, 0.05).
narrative_ontology:measurement(tran_tr_t1955, transition_causality__overdetermined_collapse_reading, theater_ratio, 1955, 0.08).
narrative_ontology:measurement(tran_tr_t1965, transition_causality__overdetermined_collapse_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(tran_tr_t1973, transition_causality__overdetermined_collapse_reading, theater_ratio, 1973, 0.1).

% Extraction over time
narrative_ontology:measurement(tran_be_t1944, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1944, 0.7).
narrative_ontology:measurement(tran_be_t1955, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1955, 0.78).
narrative_ontology:measurement(tran_be_t1965, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1965, 0.85).
narrative_ontology:measurement(tran_be_t1973, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1973, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1944, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1944, 0.8).
narrative_ontology:measurement(tran_su_t1955, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1955, 0.85).
narrative_ontology:measurement(tran_su_t1965, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1965, 0.9).
narrative_ontology:measurement(tran_su_t1973, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1973, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__overdetermined_collapse_reading, global_infrastructure).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'transition_causality' kernel, focusing on the structural inevitability of the collapse. Sibling readings include 'contingent_choice_reading' and 'hybrid_trigger_reading', which offer alternative causal interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
