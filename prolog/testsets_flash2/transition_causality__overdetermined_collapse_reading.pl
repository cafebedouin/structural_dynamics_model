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
 *   This constraint represents the structural inevitability of the collapse
 *   of the fixed exchange rate regime (e.g., Bretton Woods system), as
 *   understood through an 'overdetermined collapse' reading. It posits that
 *   multiple reinforcing contradictions, such as the Triffin Dilemma, made
 *   the transition to a floating rate system unavoidable, regardless of
 *   specific policy choices. The constraint is classified as a Mountain
 *   because its persistence and eventual breakdown are seen as governed by
 *   irreducible economic and political-economic laws, not contingent human
 *   decisions. The high extractiveness and suppression reflect the increasing
 *   costs and lack of viable alternatives for participants as the system
 *   approached its breaking point.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, 0.95).
domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, 0.98).
domain_priors:theater_ratio(transition_causality__overdetermined_collapse_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, 0.99).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__overdetermined_collapse_reading, mountain).
narrative_ontology:human_readable(transition_causality__overdetermined_collapse_reading, "Inevitable Collapse of Fixed Exchange Rate Regime (Overdetermined Reading)").
narrative_ontology:topic_domain(transition_causality__overdetermined_collapse_reading, "monetary_economics/political_economy/international_finance").

domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__overdetermined_collapse_reading, 'd47af95b-136f-4591-ad77-df838efe4eaf').
narrative_ontology:cs_kernel_codification('d47af95b-136f-4591-ad77-df838efe4eaf', implicit).
narrative_ontology:cs_authority_grounding('d47af95b-136f-4591-ad77-df838efe4eaf', diffuse_epistemic).
narrative_ontology:cs_reading_relation('d47af95b-136f-4591-ad77-df838efe4eaf', transition_causality__contingent_choice_reading, forecloses).
narrative_ontology:cs_reading_relation('d47af95b-136f-4591-ad77-df838efe4eaf', transition_causality__hybrid_trigger_reading, forecloses).
narrative_ontology:cs_axiom('d47af95b-136f-4591-ad77-df838efe4eaf', foundational, structural_contradictions_are_determinative).
narrative_ontology:cs_axiom_status(structural_contradictions_are_determinative, holdable).
narrative_ontology:cs_axiom_grounding('d47af95b-136f-4591-ad77-df838efe4eaf', structural_contradictions_are_determinative, empirically_contingent).
narrative_ontology:cs_axiom('d47af95b-136f-4591-ad77-df838efe4eaf', foundational, triffin_dilemma_is_irreducible).
narrative_ontology:cs_axiom_status(triffin_dilemma_is_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('d47af95b-136f-4591-ad77-df838efe4eaf', triffin_dilemma_is_irreducible, empirically_contingent).
narrative_ontology:cs_reference_frame('d47af95b-136f-4591-ad77-df838efe4eaf', fixed_rate_system_inherent_instability).
narrative_ontology:cs_drift_state('d47af95b-136f-4591-ad77-df838efe4eaf', post_bretton_woods_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d47af95b-136f-4591-ad77-df838efe4eaf', '').
narrative_ontology:cs_kernel_id(transition_causality__overdetermined_collapse_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, fixed_rate_regime_participants).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, national_monetary_authorities).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, international_financial_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All nations and economic actors operating under the fixed exchange rate system, particularly those whose domestic policy was constrained by the need to maintain parity. They bore the costs of the system's inherent contradictions, leading to eventual collapse.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, fixed_rate_regime_participants, payer,
    institutional, generational, trapped, global).

% Central banks and finance ministries tasked with managing their national currencies within the fixed exchange rate system. They faced increasing pressure from the Triffin Dilemma, forced to choose between domestic policy goals and international stability, ultimately unable to prevent the system's breakdown.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, national_monetary_authorities, payer,
    institutional, biographical, constrained, national).

% Organizations like the IMF, designed to manage and stabilize the international monetary system. They were structurally unable to resolve the fundamental contradictions of the fixed exchange rate regime, becoming victims of its inevitable collapse despite their efforts.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, international_financial_institutions, payer,
    institutional, generational, constrained, global).

% Academics and researchers who analyze the historical and theoretical underpinnings of international monetary systems. From this analytical seat, the collapse of the fixed exchange rate regime is viewed as a structurally determined outcome, not a contingent policy choice.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, economic_historians_and_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The fixed exchange rate regime aimed to provide global monetary stability and facilitate international trade and investment by pegging currencies to a common standard (e.g., gold or the USD).
% TRANSFER_FUNCTION: The system implicitly transferred the burden of adjustment to countries with balance of payments deficits, and eventually, the structural contradictions transferred the costs of instability to all participants as the system became unsustainable.
% ABSENT_VOICES: Actors who might have advocated for a more flexible or alternative international monetary system from the outset, but whose perspectives were marginalized by the prevailing orthodoxy of fixed exchange rates. Their warnings about inherent contradictions were not heeded until the system's collapse.
% DISAPPEARANCE_RATIONALE: From this reading, the fixed exchange rate regime was inherently unstable due to fundamental contradictions (like the Triffin Dilemma). Its 'disappearance' was not a contingent event but a structural inevitability; the world would have rearranged itself to a floating rate system regardless of specific policy choices, as the underlying economic forces dictated the transition.
% FOUNDING_PROBLEM: The need for a stable international monetary system after World War II to prevent competitive devaluations and facilitate global economic recovery.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and theorists widely corroborate that the initial problem of post-war monetary instability was addressed, but the solution itself contained inherent contradictions that led to its demise. The Triffin Dilemma, in particular, is a well-corroborated structural flaw, attested by independent academic research and historical analysis, not just by those who benefited from the system's initial stability.
narrative_ontology:disappearance_verdict(transition_causality__overdetermined_collapse_reading, world_unchanged).
narrative_ontology:founding_problem_status(transition_causality__overdetermined_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__overdetermined_collapse_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(transition_causality__overdetermined_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__overdetermined_collapse_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.95) is high because the fixed exchange rate regime, while initially beneficial, imposed increasing costs and policy constraints on participating nations due to its inherent contradictions. The Triffin Dilemma, where the reserve currency issuer's domestic needs conflict with global liquidity provision, is a core example. Suppression (0.98) is near-total because, from this reading, there were no viable structural alternatives to the eventual collapse; attempts to 'fix' the system only delayed the inevitable. The theater ratio is low (0.05) because the system was genuinely functional for a period, and its eventual failure was due to structural flaws, not performative maintenance. Accessibility collapse is near-total (0.99) and resistance is minimal (0.01) because the structural forces were overwhelming, leaving no real exit or effective resistance against the inevitable transition.
 *
 * PERSPECTIVAL GAP:
 *   Other readings (contingent_choice_reading, hybrid_trigger_reading) would emphasize human agency or specific trigger events. This reading, however, sees the structural contradictions as so profound that the system's collapse was a deterministic outcome. The divergence lies in whether the transition was a 'choice' or an 'inevitability.'
 *
 * DIRECTIONALITY LOGIC:
 *   From this 'overdetermined collapse' reading, all participants in the fixed exchange rate regime were ultimately 'victims' of its structural inevitability. National monetary authorities and international financial institutions, despite their roles in managing the system, were constrained by its inherent flaws. There are no identifiable beneficiaries in the long run, as the system's collapse imposed costs on all. The constraint acts as a Mountain, extracting from all who operate within its bounds due to its unchangeable nature.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contingency_vs_inevitability,
    'To what extent were specific policy choices or contingent events truly irrelevant to the timing and nature of the fixed exchange rate regime''s collapse, as opposed to being merely symptoms of deeper structural forces?',
    'Counterfactual historical analysis using agent-based modeling or comparative case studies of similar systems under different policy regimes, if such data were available.',
    'If contingent choices are found to have significant causal weight, the constraint''s classification might shift towards a Tangled Rope or Snare, reflecting human agency and potential for alternative outcomes. If the inevitability holds, its Mountain classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contingency_vs_inevitability, conceptual, 'Ambiguity regarding the degree of structural determinism versus contingent agency in the system''s collapse.').

omega_variable(
    triffin_dilemma_naturalness,
    'Is the Triffin Dilemma an irreducible ''natural law'' of international finance, or a historically contingent structural feature that could be overcome by different institutional designs?',
    'Theoretical advancements in international monetary economics proposing and validating alternative reserve currency systems that demonstrably resolve the dilemma without creating new, equally severe contradictions.',
    'If the Triffin Dilemma is found to be contingent, the ''emerges_naturally'' claim for this Mountain constraint would be weakened, potentially reclassifying it as a Snare or Tangled Rope, as its ''naturalness'' would be revealed as a constructed feature benefiting certain actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_dilemma_naturalness, conceptual, 'Whether the core contradiction driving the collapse is a fundamental economic law or a design flaw.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__overdetermined_collapse_reading, 1944, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1944, transition_causality__overdetermined_collapse_reading, theater_ratio, 1944, 0.05).
narrative_ontology:measurement(tran_tr_t1955, transition_causality__overdetermined_collapse_reading, theater_ratio, 1955, 0.05).
narrative_ontology:measurement(tran_tr_t1965, transition_causality__overdetermined_collapse_reading, theater_ratio, 1965, 0.05).
narrative_ontology:measurement(tran_tr_t1973, transition_causality__overdetermined_collapse_reading, theater_ratio, 1973, 0.05).

% Extraction over time
narrative_ontology:measurement(tran_be_t1944, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1944, 0.7).
narrative_ontology:measurement(tran_be_t1955, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1955, 0.8).
narrative_ontology:measurement(tran_be_t1965, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1965, 0.9).
narrative_ontology:measurement(tran_be_t1973, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1973, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1944, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1944, 0.9).
narrative_ontology:measurement(tran_su_t1955, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1955, 0.95).
narrative_ontology:measurement(tran_su_t1965, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1965, 0.98).
narrative_ontology:measurement(tran_su_t1973, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1973, 0.99).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__overdetermined_collapse_reading, global_infrastructure).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'transition_causality' kernel, focusing on the structural inevitability of the fixed exchange rate regime's collapse. It is linked to other readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
