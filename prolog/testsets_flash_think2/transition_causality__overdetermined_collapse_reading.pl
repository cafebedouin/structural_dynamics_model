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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: transition_causality__overdetermined_collapse_reading
 *   human_readable: Inevitable Collapse of Fixed Exchange Rate Regime (Overdetermined Reading)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'overdetermined_collapse_reading'
 *   of the 'transition_causality' kernel, asserting that the collapse of the
 *   fixed exchange rate regime (e.g., Bretton Woods) was structurally
 *   inevitable due to multiple reinforcing contradictions, such as the
 *   Triffin Dilemma. The constraint is the *inevitability* itself, which
 *   operated as a natural law-like force, extracting resources from those
 *   attempting to resist it. Efforts to maintain the system became
 *   increasingly performative and suppressive against these inherent forces.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, 0.85).
domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, 0.9).
domain_priors:theater_ratio(transition_causality__overdetermined_collapse_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__overdetermined_collapse_reading, mountain).
narrative_ontology:human_readable(transition_causality__overdetermined_collapse_reading, "Inevitable Collapse of Fixed Exchange Rate Regime (Overdetermined Reading)").
narrative_ontology:topic_domain(transition_causality__overdetermined_collapse_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(transition_causality__overdetermined_collapse_reading).
domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__overdetermined_collapse_reading, 'fee16ff8-5262-4709-9719-27d75e2942a0').
narrative_ontology:cs_kernel_codification('fee16ff8-5262-4709-9719-27d75e2942a0', formalized).
narrative_ontology:cs_authority_grounding('fee16ff8-5262-4709-9719-27d75e2942a0', expertise).
narrative_ontology:cs_interpretation_layer_present('fee16ff8-5262-4709-9719-27d75e2942a0').
narrative_ontology:cs_reading_relation('fee16ff8-5262-4709-9719-27d75e2942a0', transition_causality__contingent_choice_reading, forecloses).
narrative_ontology:cs_reading_relation('fee16ff8-5262-4709-9719-27d75e2942a0', transition_causality__hybrid_trigger_reading, forecloses).
narrative_ontology:cs_axiom('fee16ff8-5262-4709-9719-27d75e2942a0', foundational, structural_contradictions_lead_to_collapse).
narrative_ontology:cs_axiom_status(structural_contradictions_lead_to_collapse, holdable).
narrative_ontology:cs_axiom_grounding('fee16ff8-5262-4709-9719-27d75e2942a0', structural_contradictions_lead_to_collapse, empirically_contingent).
narrative_ontology:cs_axiom('fee16ff8-5262-4709-9719-27d75e2942a0', foundational, triffin_dilemma_inherent_instability).
narrative_ontology:cs_axiom_status(triffin_dilemma_inherent_instability, holdable).
narrative_ontology:cs_axiom_grounding('fee16ff8-5262-4709-9719-27d75e2942a0', triffin_dilemma_inherent_instability, empirically_contingent).
narrative_ontology:cs_reference_frame('fee16ff8-5262-4709-9719-27d75e2942a0', bretton_woods_fixed_parity_system).
narrative_ontology:cs_drift_state('fee16ff8-5262-4709-9719-27d75e2942a0', post_triffin_dilemma_articulation, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('fee16ff8-5262-4709-9719-27d75e2942a0', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(transition_causality__overdetermined_collapse_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, central_banks).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, governments_defending_parities).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, international_traders).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, domestic_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, international_speculators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Tasked with defending fixed exchange rate parities, they bore the costs of intervention, depleting reserves and sacrificing domestic monetary policy autonomy in a losing battle against structural contradictions.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, central_banks, payer,
    institutional, immediate, constrained, global).

% Committed to the fixed exchange rate system, they enacted policies and diplomatic efforts to maintain it, often at the expense of national economic goals, believing the system could be preserved through political will.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, governments_defending_parities, agenda_setter,
    institutional, biographical, constrained, national).

% Operated within a system of increasing volatility and uncertainty, facing rising transaction costs and risks as the fixed parities became harder to defend, yet were dependent on international trade for their livelihoods.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, international_traders, payer,
    moderate, immediate, constrained, global).

% Were subject to the economic distortions caused by the fixed exchange rate regime, such as trade imbalances and capital controls, which impacted their competitiveness and access to finance, with limited ability to influence policy.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, domestic_industries, payer,
    organized, biographical, constrained, national).

% Analyzed the inherent contradictions of the system, such as the Triffin Dilemma, and increasingly articulated the structural inevitability of its collapse, providing the intellectual framework for this 'overdetermined' reading.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, academic_economists, observer,
    analytical, generational, analytical, universal).

% Profited from anticipating the inevitable collapse, exploiting the growing inconsistencies and vulnerabilities of the fixed exchange rate system through currency speculation, accelerating the very transition that was structurally determined.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, international_speculators, beneficiary,
    powerful, immediate, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__overdetermined_collapse_reading, diffuse).
narrative_ontology:fixing_cost_class(transition_causality__overdetermined_collapse_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The fixed exchange rate regime aimed to provide international monetary stability, facilitating trade and investment by eliminating currency risk among participating nations.
% TRANSFER_FUNCTION: The system transferred economic flexibility and national monetary autonomy from participating nations (especially deficit countries) to the maintenance of fixed parities, ultimately extracting resources from those attempting to defend an unsustainable structure.
% ABSENT_VOICES: Advocates for flexible exchange rates or alternative international monetary systems were largely excluded from the core decision-making bodies, their warnings often dismissed until the contradictions became undeniable.
% DISAPPEARANCE_RATIONALE: The collapse of the fixed exchange rate regime led to a fundamental reorganization of the international monetary system, shifting towards floating exchange rates for major currencies and altering global trade and capital flows. The inevitability of this transition meant the old system could not persist.
% FOUNDING_PROBLEM: The problem of establishing a stable international monetary order after World War II, preventing competitive devaluations and fostering global economic recovery.
% FOUNDING_PROBLEM_CORROBORATION: Academic economists and historical analyses widely corroborate that the founding problem of post-war monetary stability was addressed by the Bretton Woods system, but that system's inherent contradictions (e.g., Triffin Dilemma) rendered its long-term stability impossible, leading to its eventual collapse. The problem of *maintaining* fixed rates became dead as the system itself became unsustainable.
narrative_ontology:disappearance_verdict(transition_causality__overdetermined_collapse_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__overdetermined_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__overdetermined_collapse_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The high extractiveness (0.85) reflects the increasing costs borne by nations and central banks trying to defend unsustainable parities against fundamental economic forces. Suppression (0.90) was required to prevent market forces from breaking the fixed rates, including capital controls and interventions. The high theater ratio (0.70) indicates that by the end of the period, much of the activity to maintain the system was performative, designed to project stability rather than address the underlying inevitability of collapse. Accessibility collapse (0.95) is high because, within the framework of the fixed-rate regime, no viable alternative to eventual collapse existed; the structural contradictions foreclosed other outcomes. Resistance (0.80) was high from market forces and actors trying to escape the constraints of the failing system.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of central banks and governments, the system was a coordination mechanism that required defense, even if increasingly costly. From the 'overdetermined collapse' reading, these efforts were ultimately futile, and the system was extracting from them as it moved towards its inevitable end. Academic economists, particularly those articulating the Triffin Dilemma, saw the structural inevitability, while policymakers often clung to the belief that political will could overcome economic laws.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain of inevitability, the constraint itself doesn't have beneficiaries in the sense of collecting rents. Instead, it imposes costs on all actors attempting to operate within or defend the structurally unstable fixed-rate regime. Central banks and governments were direct targets, bearing the costs of defense. International traders and domestic industries were also victims, facing distortions and uncertainty. International speculators, while profiting, did so by *exploiting* the inevitability, not by *benefiting from the stability* of the system, thus their role is more complex but still aligns with the inevitability's impact.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    overdetermination_vs_contingency,
    'Was the collapse of the fixed exchange rate regime truly overdetermined by structural factors, or did contingent policy choices and specific trigger events play a more decisive role?',
    'Counterfactual historical analysis: detailed modeling of alternative policy paths and their potential to avert collapse, or comparative analysis with other fixed-rate regimes that persisted longer or collapsed differently.',
    'If contingent choices were decisive, this ''overdetermined'' reading would be reclassified as a conceptual overstatement, potentially shifting the constraint''s type from Mountain to a more constructed type (e.g., Tangled Rope or Snare) for the *regime itself*, rather than its collapse being inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overdetermination_vs_contingency, conceptual, 'Ambiguity between structural inevitability and contingent policy influence in the transition.').

omega_variable(
    triffin_dilemma_natural_law_status,
    'Is the Triffin Dilemma a genuine natural law of international finance, an irreducible physical/logical limit, or a constructed constraint that could have been managed or mitigated differently with alternative institutional designs?',
    'Theoretical advancements in international monetary economics proposing stable solutions to the dilemma, or empirical observation of long-lasting international reserve currencies that do not exhibit the dilemma''s contradictions.',
    'If the Triffin Dilemma is found to be a constructed or manageable constraint, the ''emerges_naturally'' claim for this Mountain would be challenged, potentially reclassifying the constraint as a False Summit Mountain or a Snare, as its ''inevitability'' would be revealed as a consequence of specific design choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_dilemma_natural_law_status, empirical, 'The natural law status of the Triffin Dilemma and its implications for inevitability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__overdetermined_collapse_reading, 1944, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1944, transition_causality__overdetermined_collapse_reading, theater_ratio, 1944, 0.1).
narrative_ontology:measurement(tran_tr_t1955, transition_causality__overdetermined_collapse_reading, theater_ratio, 1955, 0.3).
narrative_ontology:measurement(tran_tr_t1965, transition_causality__overdetermined_collapse_reading, theater_ratio, 1965, 0.5).
narrative_ontology:measurement(tran_tr_t1970, transition_causality__overdetermined_collapse_reading, theater_ratio, 1970, 0.65).
narrative_ontology:measurement(tran_tr_t1973, transition_causality__overdetermined_collapse_reading, theater_ratio, 1973, 0.7).

% Extraction over time
narrative_ontology:measurement(tran_be_t1944, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1944, 0.4).
narrative_ontology:measurement(tran_be_t1955, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1955, 0.6).
narrative_ontology:measurement(tran_be_t1965, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1965, 0.75).
narrative_ontology:measurement(tran_be_t1970, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1970, 0.82).
narrative_ontology:measurement(tran_be_t1973, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1973, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1944, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1944, 0.5).
narrative_ontology:measurement(tran_su_t1955, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1955, 0.65).
narrative_ontology:measurement(tran_su_t1965, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1965, 0.8).
narrative_ontology:measurement(tran_su_t1970, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1970, 0.88).
narrative_ontology:measurement(tran_su_t1973, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1973, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__overdetermined_collapse_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
