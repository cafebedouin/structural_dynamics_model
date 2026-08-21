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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Inevitable Collapse of Fixed Exchange Rate Regime (Overdetermined Reading)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint represents the 'overdetermined collapse' reading of the
 *   transition causality kernel, specifically focusing on the breakdown of
 *   the Bretton Woods fixed exchange rate system. It posits that the
 *   transition was structurally inevitable due to multiple reinforcing
 *   contradictions, such as the Triffin Dilemma, which made the system
 *   inherently unstable and counterfactually unviable. The constraint is
 *   claimed as a Mountain, reflecting its perceived natural law-like
 *   inevitability, even as its high extractiveness and suppression indicate
 *   the severe costs imposed on actors within the system.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, 0.85).
domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, 0.9).
domain_priors:theater_ratio(transition_causality__overdetermined_collapse_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__overdetermined_collapse_reading, mountain).
narrative_ontology:human_readable(transition_causality__overdetermined_collapse_reading, "Inevitable Collapse of Fixed Exchange Rate Regime (Overdetermined Reading)").
narrative_ontology:topic_domain(transition_causality__overdetermined_collapse_reading, "monetary_economics/political_economy/international_finance").

domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__overdetermined_collapse_reading, '1ead68b7-fc69-4c98-ab9c-b5a586882106').
narrative_ontology:cs_kernel_codification('1ead68b7-fc69-4c98-ab9c-b5a586882106', formalized).
narrative_ontology:cs_authority_grounding('1ead68b7-fc69-4c98-ab9c-b5a586882106', diffuse_epistemic).
narrative_ontology:cs_reading_relation('1ead68b7-fc69-4c98-ab9c-b5a586882106', transition_causality__contingent_choice_reading, coexists_with).
narrative_ontology:cs_reading_relation('1ead68b7-fc69-4c98-ab9c-b5a586882106', transition_causality__hybrid_trigger_reading, coexists_with).
narrative_ontology:cs_axiom('1ead68b7-fc69-4c98-ab9c-b5a586882106', foundational, structural_contradictions_accumulate_inevitably).
narrative_ontology:cs_axiom_status(structural_contradictions_accumulate_inevitably, holdable).
narrative_ontology:cs_axiom_grounding('1ead68b7-fc69-4c98-ab9c-b5a586882106', structural_contradictions_accumulate_inevitably, empirically_contingent).
narrative_ontology:cs_axiom('1ead68b7-fc69-4c98-ab9c-b5a586882106', foundational, counterfactual_viability_of_fixed_regime_near_zero).
narrative_ontology:cs_axiom_status(counterfactual_viability_of_fixed_regime_near_zero, holdable).
narrative_ontology:cs_axiom_grounding('1ead68b7-fc69-4c98-ab9c-b5a586882106', counterfactual_viability_of_fixed_regime_near_zero, empirically_contingent).
narrative_ontology:cs_reference_frame('1ead68b7-fc69-4c98-ab9c-b5a586882106', bretton_woods_fixed_exchange_rate_system).
narrative_ontology:cs_drift_state('1ead68b7-fc69-4c98-ab9c-b5a586882106', pre_nixon_shock_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('1ead68b7-fc69-4c98-ab9c-b5a586882106', '').
narrative_ontology:cs_kernel_id(transition_causality__overdetermined_collapse_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, actors_constrained_by_fixed_rate_regime).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, global_financial_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are national governments, central banks, and businesses operating under the Bretton Woods fixed exchange rate system. They are forced to contend with the accumulating contradictions (e.g., Triffin Dilemma) that make the system unsustainable, leading to inevitable disruption and loss of stability.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, actors_constrained_by_fixed_rate_regime, payer,
    powerless, biographical, trapped, global).

% The overarching structure of international finance, which undergoes a forced, disruptive transition from a fixed-rate to a floating-rate regime. It bears the systemic costs of instability and uncertainty during the collapse, even if a new equilibrium eventually emerges.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, global_financial_system, payer,
    institutional, generational, constrained, global).

% Organizations like the IMF, tasked with maintaining the international monetary order. From this reading, they are ultimately unable to prevent the structural inevitability of the collapse, despite their efforts to manage the system. They administer the transition rather than control its fundamental cause.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, international_monetary_institutions, agenda_setter,
    institutional, generational, analytical, global).

% Scholars who analyze the underlying structural contradictions of the international monetary system. This reading aligns with those who identified the Triffin Dilemma and other fundamental imbalances as making the fixed-rate regime inherently unstable and its collapse inevitable.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, academic_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None directly; the constraint describes the inevitable breakdown of a prior coordination mechanism (the Bretton Woods fixed exchange rate system) due to its internal contradictions. The fixed-rate regime itself coordinated global trade and finance.
% TRANSFER_FUNCTION: The constraint facilitates the transfer of stability and predictability from the fixed-rate regime to a new, more volatile, or differently structured regime. This transition often involves significant wealth redistribution and economic disruption as the system rebalances.
% ABSENT_VOICES: Those who believed in the long-term viability of the fixed-rate regime or who proposed alternative, non-transitional policy solutions; their perspectives were structurally marginalized by the accumulating contradictions that rendered such solutions ineffective against the inevitable forces.
% DISAPPEARANCE_RATIONALE: If the structural contradictions leading to the collapse were somehow removed, the global financial system would not have undergone the profound reordering it did in the early 1970s, and the subsequent monetary order would be fundamentally different. The 'disappearance' of this inevitability would mean a different history.
% FOUNDING_PROBLEM: The need for a stable international monetary order after WWII, balancing national monetary policy autonomy with fixed exchange rates to prevent competitive devaluations and foster global trade.
% FOUNDING_PROBLEM_CORROBORATION: Historical accounts, economic analyses from various schools of thought (including those critical of the fixed-rate regime), and official reports from international bodies all corroborate the initial problem and the eventual structural pressures that led to the system's demise. The problem the system was built to solve was superseded by new, internal contradictions.
narrative_ontology:disappearance_verdict(transition_causality__overdetermined_collapse_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__overdetermined_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__overdetermined_collapse_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(transition_causality__overdetermined_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__overdetermined_collapse_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.85) and suppression (0.90) reflect the severe, unavoidable costs imposed by the accumulating structural contradictions, leading to a disruptive collapse. The 'inevitability' means alternatives were suppressed, and the system extracted stability and predictability from its participants. Accessibility collapse is near-total (0.95) because, from this reading, no policy choices could have averted the fundamental structural forces. Resistance (0.70) is high because actors attempted to maintain the system, but their efforts were ultimately futile against the overdetermined forces. The low theater ratio (0.10) indicates that the constraint is not maintained by performance but by genuine, albeit destructive, structural dynamics.
 *
 * PERSPECTIVAL GAP:
 *   This reading stands in contrast to those that emphasize policy choices or contingent trigger events. From this perspective, the structural forces were so overwhelming that individual or collective agency could not have altered the fundamental trajectory. Other readings would see more degrees of freedom for policymakers or a greater role for specific events in precipitating the collapse.
 *
 * DIRECTIONALITY LOGIC:
 *   From this 'overdetermined collapse' reading, all actors operating within the fixed-rate regime are effectively 'victims' of the inevitable transition. The constraint (the structural inevitability of collapse) extracts from them by forcing a disruptive reordering of the global financial system. There are no direct beneficiaries of the *inevitability* itself, only those who might adapt better or profit from the *new* regime that emerges post-collapse. International monetary institutions, while agenda-setters for the system, are also constrained by these structural forces.
 *
 * MANDATROPHY ANALYSIS:
 *   The concept of mandatrophy doesn't directly apply to a 'mountain' of structural inevitability. The constraint here is the *process of inevitable collapse*, not a human-designed mandate that could atrophy. The Bretton Woods system itself had a mandate, which ultimately failed due to these structural contradictions, leading to its 'death' rather than atrophy in the conventional sense. The engine's detection of a claimed Mountain with high extractiveness will correctly flag this as a potential 'false summit' – a structural force presented as natural law, but with significant costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    triffin_dilemma_naturalness,
    'Is the Triffin Dilemma (the inherent conflict between a national currency serving as an international reserve currency and maintaining domestic monetary policy goals) a genuine natural law of international finance, or a constructed feature of a specific monetary system?',
    'Comparative analysis of alternative international monetary architectures (e.g., a global reserve currency, a basket of currencies) to determine if similar contradictions emerge under different structural designs.',
    'If a genuine natural law, it reinforces the ''mountain'' claim for the inevitability of collapse. If a constructed feature, it suggests that different institutional designs could have averted or mitigated the dilemma, weakening the inevitability claim and reclassifying the constraint as a Snare or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_dilemma_naturalness, conceptual, 'Ambiguity regarding the naturalness of the Triffin Dilemma''s structural contradictions.').

omega_variable(
    contingent_choice_vs_inevitability,
    'To what extent could different policy choices (e.g., earlier revaluation of currencies, stricter capital controls, greater international cooperation) have averted or significantly delayed the collapse, as argued by the ''contingent_choice_reading''?',
    'Detailed counterfactual historical analysis, employing economic modeling and archival research to simulate alternative policy paths and their outcomes.',
    'If counterfactual analysis shows viable alternative paths, it would weaken the ''overdetermined collapse'' reading''s claim of inevitability, shifting the constraint towards a ''Tangled Rope'' or ''Snare'' where agency played a larger role. If no viable alternatives are found, it strengthens the ''mountain'' claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contingent_choice_vs_inevitability, empirical, 'The degree to which policy choices could have altered the transition''s trajectory.').

omega_variable(
    hybrid_trigger_role,
    'Did the structural contradictions, while accumulating, require specific contingent trigger events (e.g., Nixon Shock, oil crisis) to actualize the collapse, as argued by the ''hybrid_trigger_reading''?',
    'Event-history analysis and qualitative process tracing to identify the precise causal role of specific events in the final breakdown, distinguishing between necessary conditions and sufficient triggers.',
    'If specific triggers are found to be necessary, it would introduce a ''contingent'' element to the collapse, moving the constraint away from pure ''mountain'' towards a ''Tangled Rope'' where structural forces interact with specific events. If triggers are merely symptoms, the ''mountain'' claim is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_trigger_role, empirical, 'The role of contingent triggers versus pure structural inevitability in the collapse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__overdetermined_collapse_reading, 1960, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1960, transition_causality__overdetermined_collapse_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(tran_tr_t1963, transition_causality__overdetermined_collapse_reading, theater_ratio, 1963, 0.1).
narrative_ontology:measurement(tran_tr_t1966, transition_causality__overdetermined_collapse_reading, theater_ratio, 1966, 0.1).
narrative_ontology:measurement(tran_tr_t1969, transition_causality__overdetermined_collapse_reading, theater_ratio, 1969, 0.1).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__overdetermined_collapse_reading, theater_ratio, 1971, 0.1).

% Extraction over time
narrative_ontology:measurement(tran_be_t1960, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1960, 0.6).
narrative_ontology:measurement(tran_be_t1963, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1963, 0.65).
narrative_ontology:measurement(tran_be_t1966, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1966, 0.72).
narrative_ontology:measurement(tran_be_t1969, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1969, 0.8).
narrative_ontology:measurement(tran_be_t1971, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1971, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1960, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1960, 0.7).
narrative_ontology:measurement(tran_su_t1963, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1963, 0.75).
narrative_ontology:measurement(tran_su_t1966, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1966, 0.8).
narrative_ontology:measurement(tran_su_t1969, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1969, 0.85).
narrative_ontology:measurement(tran_su_t1971, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1971, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__overdetermined_collapse_reading, global_infrastructure).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, post_bretton_woods_floating_rates).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, us_dollar_hegemony).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, international_capital_mobility).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'transition_causality' kernel, which describes the forces leading to major international monetary regime shifts. This 'overdetermined_collapse_reading' emphasizes structural inevitability, while sibling readings ('contingent_choice_reading', 'hybrid_trigger_reading') offer alternative causal accounts. All readings are linked within the 'transition_causality' constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
