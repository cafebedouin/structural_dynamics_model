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
 *   This constraint story analyzes the Bretton Woods system's collapse
 *   through the 'hybrid trigger' reading of transition causality. It posits
 *   that while fundamental structural contradictions (like the Triffin
 *   Dilemma) made the system inherently unstable, its actual breakdown in
 *   1971 required specific contingent events—such as the fiscal shock of the
 *   Vietnam War and the French demands for gold convertibility—to act as
 *   triggers. The system is classified as a Tangled Rope because it provided
 *   genuine coordination (stable exchange rates) but also enabled asymmetric
 *   extraction by the US, requiring active enforcement to maintain dollar
 *   supremacy.
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
narrative_ontology:cs_story_uid(transition_causality__hybrid_trigger_reading, '3d4a1081-7647-49fe-9336-c56d585aa031').
narrative_ontology:cs_kernel_codification('3d4a1081-7647-49fe-9336-c56d585aa031', formalized).
narrative_ontology:cs_authority_grounding('3d4a1081-7647-49fe-9336-c56d585aa031', lineage).
narrative_ontology:cs_interpretation_layer_present('3d4a1081-7647-49fe-9336-c56d585aa031').
narrative_ontology:cs_reading_relation('3d4a1081-7647-49fe-9336-c56d585aa031', transition_causality__contingent_choice_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d4a1081-7647-49fe-9336-c56d585aa031', transition_causality__overdetermined_collapse_reading, coexists_with).
narrative_ontology:cs_axiom('3d4a1081-7647-49fe-9336-c56d585aa031', foundational, structural_contradictions_accumulate).
narrative_ontology:cs_axiom_status(structural_contradictions_accumulate, holdable).
narrative_ontology:cs_axiom_grounding('3d4a1081-7647-49fe-9336-c56d585aa031', structural_contradictions_accumulate, empirically_contingent).
narrative_ontology:cs_axiom('3d4a1081-7647-49fe-9336-c56d585aa031', foundational, contingent_triggers_actualize_collapse).
narrative_ontology:cs_axiom_status(contingent_triggers_actualize_collapse, holdable).
narrative_ontology:cs_axiom_grounding('3d4a1081-7647-49fe-9336-c56d585aa031', contingent_triggers_actualize_collapse, empirically_contingent).
narrative_ontology:cs_reference_frame('3d4a1081-7647-49fe-9336-c56d585aa031', post_wwii_monetary_stability).
narrative_ontology:cs_drift_state('3d4a1081-7647-49fe-9336-c56d585aa031', vietnam_war_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3d4a1081-7647-49fe-9336-c56d585aa031', '').
narrative_ontology:cs_kernel_id(transition_causality__hybrid_trigger_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, us_treasury).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, us_military_industrial_complex).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, foreign_central_banks).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, global_south_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administered the Bretton Woods system, benefiting from the dollar's reserve currency status and seigniorage. Faced the Triffin Dilemma but resisted revaluation or gold convertibility changes until forced. Its actions (or inactions) were central to the system's maintenance and eventual breakdown.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, us_treasury, agenda_setter,
    institutional, generational, constrained, global).

% Required to hold dollar reserves, effectively financing US deficits. Faced the dilemma of either holding depreciating dollars or demanding gold, which would collapse the system. Their collective action problem prevented early exit, but individual actions (like French gold demands) acted as triggers.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, foreign_central_banks, payer,
    organized, biographical, constrained, global).

% Dependent on the stability of the international monetary system but had no voice in its governance or the decisions leading to its collapse. Suffered disproportionately from the instability and inflation that followed the breakdown.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, global_south_economies, payer,
    powerless, generational, trapped, global).

% Benefited from the ability of the US to run large external deficits (financed by dollar holdings abroad) to fund military interventions (e.g., Vietnam War) without immediate domestic economic consequences. This spending contributed to the structural pressure on the dollar.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, us_military_industrial_complex, beneficiary,
    institutional, generational, arbitrage, global).

% Designed to oversee the Bretton Woods system and facilitate international monetary cooperation. Its warnings about the Triffin Dilemma were largely unheeded by the US, highlighting its limited power to enforce structural changes.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a stable international monetary system based on fixed exchange rates pegged to the US dollar, which was convertible to gold, facilitating global trade and investment after WWII.
% TRANSFER_FUNCTION: Transferred seigniorage benefits and the ability to run persistent external deficits to the US, financed by other nations' accumulation of dollar reserves, while transferring the risk of dollar depreciation to foreign central banks.
% ABSENT_VOICES: Developing nations and smaller economies, whose stability was deeply affected by the system but who had no meaningful input into its design or the decisions that led to its collapse. They would have advocated for a more equitable and less dollar-centric system.
% DISAPPEARANCE_RATIONALE: The collapse of Bretton Woods led to a shift to floating exchange rates, increased currency volatility, and a search for new international monetary arrangements. The global financial architecture fundamentally reorganized, demonstrating the system's profound structural impact.
% FOUNDING_PROBLEM: To prevent a return to the competitive devaluations and trade wars of the interwar period, and to establish a stable framework for post-WWII economic reconstruction and growth.
% FOUNDING_PROBLEM_CORROBORATION: While the initial problem of competitive devaluations was solved, the system's internal contradictions (Triffin Dilemma) meant it could not sustain its original mandate. Economists and historians widely corroborate that the system's design flaws made its long-term stability untenable, even if the timing of its collapse was contingent.
narrative_ontology:disappearance_verdict(transition_causality__hybrid_trigger_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__hybrid_trigger_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__hybrid_trigger_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(transition_causality__hybrid_trigger_reading, 'none', 1).

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
 *   Extractiveness increased over time as the US leveraged its reserve currency status to fund deficits, effectively exporting inflation. Suppression was high because foreign central banks had limited options to exit the dollar standard without destabilizing the global economy. Theater ratio rose as the US maintained the 'dollar as good as gold' fiction despite dwindling gold reserves. The hybrid trigger reading emphasizes that while the structural pressures were building, the specific timing and nature of the collapse were not fully predetermined.
 *
 * PERSPECTIVAL GAP:
 *   The US Treasury, as the agenda-setter, experienced the system as a necessary coordination mechanism that afforded it unique policy flexibility. Foreign central banks and global south economies, however, increasingly experienced it as an extractive arrangement that imposed costs and risks without commensurate benefits. This divergence in experience is central to the hybrid trigger narrative, where the US's policy choices (e.g., funding Vietnam) exacerbated structural tensions, leading to the contingent triggers.
 *
 * DIRECTIONALITY LOGIC:
 *   The US Treasury and the US military-industrial complex were primary beneficiaries, leveraging the dollar's status. Foreign central banks and global south economies were victims, bearing the costs of dollar overvaluation and inflation. The IMF, while an observer, had limited power to alter the system's trajectory, highlighting the US's dominant position.
 *
 * MANDATROPHY ANALYSIS:
 *   The Bretton Woods system's mandate was to provide global monetary stability. While it initially fulfilled this, the hybrid trigger reading suggests that by the late 1960s, its coordination function was increasingly overshadowed by its extractive elements, particularly for the US. The system's persistence beyond its functional viability was due to the US's institutional power and the collective action problem faced by other nations, making it a Tangled Rope rather than a pure Rope or a fully atrophied Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_trigger_timing,
    'How robust was the system to different contingent triggers? Would it have collapsed at a different time or in a different manner with alternative fiscal shocks or gold demands?',
    'Detailed counterfactual historical analysis and agent-based modeling exploring alternative policy paths and external events.',
    'If highly sensitive to triggers, it strengthens the ''hybrid trigger'' reading. If collapse was imminent regardless of specific triggers, it leans towards the ''overdetermined collapse'' reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_trigger_timing, empirical, 'The sensitivity of the system''s collapse to specific contingent events.').

omega_variable(
    triffin_dilemma_inevitability,
    'Was the Triffin Dilemma an absolutely inevitable structural contradiction, or could policy innovations (e.g., SDRs earlier) have mitigated its impact sufficiently to prevent collapse?',
    'Historical analysis of proposed reforms and their feasibility, and theoretical modeling of alternative international monetary designs.',
    'If the dilemma was truly inescapable, it strengthens the structural inevitability aspect. If it was manageable with different policy choices, it lends more weight to the ''contingent choice'' reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(triffin_dilemma_inevitability, conceptual, 'The degree of inevitability of the Triffin Dilemma''s destabilizing effects.').


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
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, transition_causality__contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, transition_causality__overdetermined_collapse_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'transition_causality' kernel, focusing on the hybrid role of structural contradictions and contingent triggers in the Bretton Woods collapse. It is linked to sibling readings that emphasize contingent policy choices or overdetermined structural inevitability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
